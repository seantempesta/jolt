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
         (let ([pid (if (= (#%$top-level-value '$active-threads) 1) (fork) -2)])
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
              (in-child-now #(= 0 (in-child parked-go))) 0)))

(if (empty? @failures)
  (do (println "FORK-CHILD-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
