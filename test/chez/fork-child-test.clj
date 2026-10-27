;; A process forked from a jolt program gets only the bookkeeping of the runtime it forked from,
;; none of its threads or waits. jolt.host/after-fork-child!, called first in the child, drops it:
;; a go that parks is resumed, a timeout fires, the poller's table starts afresh, and the thread
;; table names only the threads that exist. Without it each check's child hangs (exit 2 below), as
;; the forked agents of a Seon root did (2026-10-22). The forks are made where a program that
;; forks makes them: on its main carrier (jolt.host/main-carrier-run!), its one thread, with its
;; fibers, timers and poller rounds mid-flight. A fork beside live runtime threads (carriers, the
;; timer, the poller's) is refused (-2) whenever one of them is active at that instant, so this
;; test forked from such a process only when it won that race (main-carrier-returns-test.clj).
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

;; fork only while this is the one active Chez thread, as a forking program must
(def fork-alone
  (scm/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)])
       (lambda (reset?)
         (disable-interrupts)
         (mutex-acquire #%$tc-mutex)
         (let ([pid (if (= (jolt-active-threads) 1) (fork) -2)])
           (mutex-release #%$tc-mutex)
           (enable-interrupts)
           (when (and (= pid 0) reset?) (jolt-after-fork-child!))
           pid)))"))
(def wait-exit
  (scm/eval-string
    "(let ([waitpid (foreign-procedure \"waitpid\" (int uptr int) int)])
       (lambda (pid)
         (let ([b (foreign-alloc 4)])
           (waitpid pid b 0)
           (let ([st (foreign-ref 'int b 0)]) (foreign-free b) (bitwise-and (bitwise-arithmetic-shift-right st 8) 255)))))"))
(def exit-now (scm/eval-string "(foreign-procedure \"_exit\" (int) void)"))

(defn in-child
  "The exit status of a child that runs (f) with a 5 s deadline: 0 true, 1 anything else,
  2 no answer (the child's runtime threads are gone). -2: the fork was refused."
  [f]
  (let [pid (fork-alone true)]
    (cond (zero? pid) (exit-now (let [r (deref (future (f)) 5000 ::hung)] (cond (= ::hung r) 2 (true? r) 0 :else 1)))
          (neg? pid) pid
          :else (wait-exit pid))))

;; A runtime mutex held, at the fork instant, by a thread the fork does not copy: the timer
;; thread's timed wait on timeout-mu fires and takes the mutex back, then waits to rejoin the
;; active threads, which the forking thread's hold on $tc-mutex stops, so the fork counts one
;; active thread and the child gets timeout-mu held by nobody it has. Made here at will: a
;; thread takes timeout-mu and sleeps in a collect-safe call (inactive, holding it) while the
;; parent forks. The child's alarm ends a hang; the answer is its raw wait status (0: exited 0,
;; 14: killed by SIGALRM, hung).
(def fork-beside-a-held-timeout-mu
  (scm/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)]
           [alarm (foreign-procedure \"alarm\" (unsigned) unsigned)]
           [nap (foreign-procedure __collect_safe \"usleep\" (unsigned) int)]
           [spin (foreign-procedure \"usleep\" (unsigned) int)]
           [waitpid (foreign-procedure \"waitpid\" (int uptr int) int)]
           [exit-now (foreign-procedure \"_exit\" (int) void)])
       (lambda ()
         (fork-thread (lambda () (mutex-acquire timeout-mu) (nap 400000) (mutex-release timeout-mu)))
         (spin 100000)
         (disable-interrupts)
         (mutex-acquire #%$tc-mutex)
         (let ([pid (if (= (jolt-active-threads) 1) (fork) -2)])
           (mutex-release #%$tc-mutex)
           (enable-interrupts)
           (cond ((= pid 0) (alarm 5) (jolt-after-fork-child!)
                            (jolt-timer-at! 0 (lambda () #t))
                            (exit-now 0))
                 ((< pid 0) pid)
                 (else (let ([b (foreign-alloc 4)])
                         (waitpid pid b 0)
                         (let ([st (foreign-ref 'int b 0)]) (foreign-free b) st)))))))"))

(defn parked-go [] (let [c (a/chan) r (a/go (a/<! c))] (a/>!! c 41) (= 41 (a/<!! r))))

(require '[jolt.async.remote :as remote])
(defn remote-echo
  "A value through a fresh jolt.async.remote pair with fiber pumps, whose readiness the poller
  reports: true when it comes back."
  []
  (let [[fa fb] (remote/socketpair) ea (remote/open fa {:pump :fiber}) eb (remote/open fb {:pump :fiber})]
    (a/>!! (:out ea) 7)
    (let [v (a/<!! (:in eb))] (remote/close! ea) (remote/close! eb) (= 7 v))))


(defn in-child-now
  "The exit status of a child that runs (f) on its one thread, no deadline: 0 true, 1 else."
  [f]
  (let [pid (fork-alone true)]
    (cond (zero? pid) (exit-now (if (true? (f)) 0 1))
          (neg? pid) pid
          :else (wait-exit pid))))


(jolt.host/main-carrier-run!
  (fn []
    (check-eq "the parent's go blocks run" (parked-go) true)
    (a/<!! (a/timeout 1))
    (check-eq "a go that parks is resumed in the child" (in-child parked-go) 0)
    (check-eq "a timeout fires in the child" (in-child #(= :t (a/alt!! (a/timeout 10) :t))) 0)
    (check-eq "the child's thread table names no thread of the parent's"
              (in-child #(not-any? (fn [t] (re-find #"jolt-carrier" (.getName ^Thread t))) (keys (Thread/getAllStackTraces))))
              0)

    (check-eq "the parent's poller runs (fiber pumps on a socket)" (remote-echo) true)
    (check-eq "a child of a process whose poller ran waits on a socket: its table starts afresh, not on the parent's dead kqueue"
              (in-child remote-echo) 0)

    (check-eq "the reset starts no thread: the child is the one thread it forked as"
              (in-child-now #(= 1 (count (Thread/getAllStackTraces)))) 0)
    (check-eq "a child that forks again (a double fork) gives a grandchild whose parked go resumes"
              (in-child-now #(= 0 (in-child parked-go))) 0)
    (check-eq "a child forked while a thread it does not get holds the timer's mutex sets a deadline: its runtime mutexes are its own"
              (fork-beside-a-held-timeout-mu) 0)
    ;; the parent's fibers are the parent's: one parked on a channel when the parent forked is
    ;; never run in the child, though the child puts to that channel (every fork of a Seon root
    ;; ran root's go loops on its copy of root's state)
    (let [c (a/chan) ran (atom nil)]
      (a/go (reset! ran [:the-parents-fiber-ran (a/<! c)]))
      (a/<!! (a/timeout 5))
      (check-eq "a fiber parked in the parent is not run in the child when the child wakes it"
                (in-child-now #(do (a/>!! c :from-the-child) (a/<!! (a/timeout 50)) (nil? @ran))) 0)
      (a/close! c))
    ;; the parent's shutdown hooks are the parent's: a child that exits through System/exit runs
    ;; none of them (a Seon root's hook stops root; every fork of root ran it on a copy of root's
    ;; state), and runs the ones it registers itself, once
    (let [f (java.io.File/createTempFile "fork-hook" ".txt") path (.getPath f)
          _ (.delete f)
          hook (Thread. #(spit path "the parent's hook ran"))]
      (.addShutdownHook (Runtime/getRuntime) hook)
      (let [pid (fork-alone true)]
        (if (zero? pid)
          (do (.addShutdownHook (Runtime/getRuntime) (Thread. #(spit (str path ".child") "the child's hook ran")))
              (System/exit 0))
          (do (wait-exit pid)
              (.removeShutdownHook (Runtime/getRuntime) hook)
              (check-eq "a child exiting through System/exit runs none of its parent's shutdown hooks"
                        (.exists (java.io.File. path)) false)
              (check-eq "and runs the one it registered itself"
                        (.exists (java.io.File. (str path ".child"))) true)
              (.delete (java.io.File. path)) (.delete (java.io.File. (str path ".child")))))))))

(if (empty? @failures)
  (do (println "FORK-CHILD-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
