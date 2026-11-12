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
                 :subprocess (a-subprocess-wait-is-not-quiescent)})))]
  (if (= {:v :ended-by-handler :heard 1 :dump? true :got :after :fork 0 :subprocess [0 false]} res)
    (println "FIBER-QUIESCENT-TEST OK")
    (do (println "FIBER-QUIESCENT-TEST FAILED" res) (System/exit 1)))
  (System/exit 0))
