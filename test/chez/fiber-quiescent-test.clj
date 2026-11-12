;; jolt.host/on-quiescent! and a remote end's pumps (jolt): a program on its main carrier whose work waits on
;; what nothing will deliver, beside a remote end whose pumps wait on their socket, is quiescent but not
;; deadlocked (the peer may still write). The registered handler hears it once, with the dump, and may end the
;; wait. And a deadlock told to the process is never told to a pump: its link keeps carrying values.
;;
;; Run: bin/jolt run test/chez/fiber-quiescent-test.clj   (prints FIBER-QUIESCENT-TEST OK)
(ns fiber-quiescent-test
  (:require [clojure.core.async :as a] [clojure.java.shell]
            [jolt.async.remote :as r]
            [jolt.scheme :as scm]))

;; A forked child holds only its own work: the fibers it copied from its parent (marked 'forked, never
;; run here) are not its program, so a child whose own work waits goes quiescent and hears it. They
;; kept it from ever being quiescent, and a child that waited for its handler hung (runtime, 2026-11-12:
;; a forked test child's on-quiescent! never fired). The child's alarm(2) ends a hang: exit by SIGALRM.
(def fork-alone
  (scm/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)])
       (lambda ()
         (disable-interrupts)
         (mutex-acquire #%$tc-mutex)
         (let ([pid (if (= (jolt-active-threads) 1) (fork) -2)])
           (mutex-release #%$tc-mutex)
           (enable-interrupts)
           (when (= pid 0) (jolt-after-fork-child!))
           pid)))"))
(def wait-exit
  (scm/eval-string
    "(let ([waitpid (foreign-procedure \"waitpid\" (int uptr int) int)])
       (lambda (pid)
         (let ([b (foreign-alloc 4)])
           (waitpid pid b 0)
           (let ([s (foreign-ref 'int b 0)]) (foreign-free b) (if (= (logand s 127) 0) (ash s -8) 99)))))"))
(def exit-now (scm/eval-string "(let ([e (foreign-procedure \"_exit\" (int) void)]) (lambda (n) (e n)))"))
(def alarm (scm/eval-string "(let ([a (foreign-procedure \"alarm\" (unsigned) unsigned)]) (lambda (n) (a n)))"))
(defn a-subprocess-wait-is-not-quiescent
  "Whether quiescence was heard while the program's one fiber waited on a subprocess's exit (sleep): the
  poller will wake it, so it is busy, not quiescent (runtime, 2026-11-12: a test child's handler ended a
  test waiting on git as a deadlock). false is right."
  []
  (let [heard (atom false)]
    (jolt.host/on-quiescent! (fn [_] (reset! heard true)))
    (let [r (clojure.java.shell/sh "sleep" "0.3")]
      (jolt.host/on-quiescent! nil)
      [(:exit r) @heard])))
(defn a-dump-names-what-holds-quiescence
  "What jolt.host/fiber-dump's quiescence line says while a fiber waits on a subprocess's exit and a
  timer is pending, and whether SIGINFO with JOLT_DUMP_DIR set writes dump-<pid>.txt holding it."
  []
  (let [dir (str (System/getProperty "java.io.tmpdir") "/fq-dump-" (System/nanoTime))
        _ (.mkdirs (java.io.File. dir))
        setenv (scm/eval-string "(let ([f (foreign-procedure \"setenv\" (string string int) int)]) (lambda (k v) (f k v 1)))")
        kill-self (scm/eval-string "(let ([k (foreign-procedure \"kill\" (int int) int)] [g (foreign-procedure \"getpid\" () int)]) (lambda (s) (k (g) s)))")
        p (promise)
        _ (a/go (a/<! (a/thread (clojure.java.shell/sh "sleep" "0.5"))) (deliver p :done))
        t (a/timeout 2000)
        _ (Thread/sleep 100)
        line (first (filter #(clojure.string/starts-with? % "quiescence:") (clojure.string/split-lines (jolt.host/fiber-dump))))]
    (setenv "JOLT_DUMP_DIR" dir)
    (kill-self 29)
    (a/<!! (a/timeout 50))
    (setenv "JOLT_DUMP_DIR" "")
    (deref p 3000 :hung)
    (let [f (java.io.File. dir (str "dump-" (.pid (java.lang.ProcessHandle/current)) ".txt"))]
      {:line line :file? (and (.exists f) (clojure.string/includes? (slurp f) "quiescence:"))})))
(defn a-dump-says-what-the-handler-did
  "The dump's quiescence line after a handler that answered, then one that threw (the program goes on: the
  waiter left is told the deadlock): [line after the first, line after the second, the waiter's end]."
  []
  (let [line #(first (filter (fn [l] (clojure.string/starts-with? l "quiescence:")) (clojure.string/split-lines (jolt.host/fiber-dump))))
        p (promise) done (promise)]
    (jolt.fibers/spawn (fn [] (deref p) (deliver done true)))
    (jolt.host/on-quiescent! (fn [_] (deliver p :go) :answered))
    (deref done)
    (let [first-line (line) q (promise) end (promise)]
      (jolt.fibers/spawn (fn [] (deliver end (try (deref q) :returned (catch Throwable t :told)))))
      (jolt.host/on-quiescent! (fn [_] (throw (ex-info "handler broke" {}))))
      ;; the deadlock is told to every waiter, this one too: it reads the waiter's end after
      (let [e (try (deref end) (catch Throwable _ (deref end)))]
        (jolt.host/on-quiescent! nil)
        [first-line (line) e]))))
(def before-next-io-wait
  ;; (before-next-io-wait f): f runs once, inside the loop's next wait on the poller (jolt-main-io-wait), which
  ;; lies between the loop's quiescence check and its last check before a deadlock
  (scm/eval-string
    "(lambda (f)
       (let ((orig jolt-main-io-wait))
         (set! jolt-main-io-wait (lambda (ms) (set! jolt-main-io-wait orig) (jolt-invoke f) (orig ms)))))"))
(defn a-thread-ending-after-the-check-is-told-first
  "What the program's one waiter gets when the only other thread runs at the loop's quiescence check and ends
  before its last check: :heard when the handler is told (tell first, then deadlock), :told when the deadlock
  came with the handler never told (the race: dump \"told this spell no\")."
  []
  (let [stop (atom false) heard (promise)
        t (doto (Thread. (fn [] (while (not @stop)))) (.start))]
    (jolt.host/on-quiescent! (fn [_] (deliver heard :heard)))
    (before-next-io-wait (fn [] (reset! stop true) (.join t)))
    (let [r (try (deref heard) (catch Throwable _ :told))]
      (jolt.host/on-quiescent! nil)
      r)))
(defn a-futures-idle-worker-is-no-work
  "Milliseconds from a fiber's park to the handler's notice, after a future ran: its pool's idle worker waits
  out a 60 s keep-alive, and it held quiescence off for all of it (read as a thread that may run)."
  []
  @(future :ran)
  (let [heard (promise) p (promise) t0 (System/currentTimeMillis)]
    (jolt.host/on-quiescent! (fn [_] (deliver p :go) (deliver heard (- (System/currentTimeMillis) t0))))
    (deref p)
    (jolt.host/on-quiescent! nil)
    (deref heard)))
(defn forked-child-hears-quiescence
  "0 when a child forked beside a parent fiber that waits hears its own quiescence; 99 when its alarm ended it."
  []
  (let [never (a/chan)]
    (a/go (a/<! never))
    (a/<!! (a/timeout 10))
    (let [pid (fork-alone)]
      (if (zero? pid)
        (do (alarm 10)
            (let [heard (promise)]
              (jolt.host/on-quiescent! (fn [_] (deliver heard true)))
              (exit-now (if (deref heard) 0 1))))
        (wait-exit pid)))))

(let [res (jolt.host/main-carrier-run!
            (fn []
              (let [[x fd] (r/remote-pair) y (r/open fd)
                    stuck (promise) heard (atom [])
                    _ (jolt.host/on-quiescent! (fn [dump] (swap! heard conj dump) (deliver stuck :ended-by-handler)))
                    v (deref stuck)
                    _ (jolt.host/on-quiescent! nil)
                    ;; a deadlock told now reaches no pump: the link still carries a value both ways
                    _ (scm/eval-string "(jolt-deadlock-tell-fibers! (jolt-deadlock-error))")
                    _ (a/>!! (:out x) :after)
                    got (a/<!! (:in y))]
                (r/close! x) (r/close! y)
                {:v v :heard (count @heard) :dump? (boolean (some #(re-find #"fiber" %) @heard)) :got got
                 :fork (forked-child-hears-quiescence)
                 :subprocess (a-subprocess-wait-is-not-quiescent)
                 :dump (a-dump-names-what-holds-quiescence)
                 :handler (a-dump-says-what-the-handler-did)
                 :race (a-thread-ending-after-the-check-is-told-first)
                 :idle (a-futures-idle-worker-is-no-work)})))
      _ (when-not (< (:idle res) 5000)
          (println "FIBER-QUIESCENT-TEST FAILED: quiescence after a future came" (:idle res) "ms after the park")
          (System/exit 1))
      res (dissoc res :idle)
      _ (when-not (= :heard (:race res))
          (println "FIBER-QUIESCENT-TEST FAILED: a thread that ended after the quiescence check:" (:race res))
          (System/exit 1))
      res (dissoc res :race)
      [l1 l2 hend] (:handler res)
      res (dissoc res :handler)
      _ (when-not (and (clojure.string/includes? (str l1) "handler set") (clojure.string/includes? (str l1) "last answer :answered")
                       (clojure.string/includes? (str l2) "last throw") (clojure.string/includes? (str l2) "handler broke") (= :told hend))
          (println "FIBER-QUIESCENT-TEST FAILED: the handler's line" (pr-str l1) "then" (pr-str l2) "waiter" hend)
          (System/exit 1))
      dump (:dump res)
      res (dissoc res :dump)
      line (or (:line dump) "")]
  (when-not (and (clojure.string/includes? line "timer") (:file? dump))
    (println "FIBER-QUIESCENT-TEST FAILED: the dump's quiescence line" (pr-str line) "file written?" (:file? dump))
    (System/exit 1))
  (if (= {:v :ended-by-handler :heard 1 :dump? true :got :after :fork 0 :subprocess [0 false]} res)
    (println "FIBER-QUIESCENT-TEST OK")
    (do (println "FIBER-QUIESCENT-TEST FAILED" res) (System/exit 1)))
  (System/exit 0))
