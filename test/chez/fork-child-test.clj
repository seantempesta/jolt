;; A process forked from a jolt program whose runtime threads were up (the fiber
;; carriers, the timer, the shutdown watcher; all parked, so one thread is active)
;; gets none of those threads, only their bookkeeping. jolt.host/after-fork-child!,
;; called first in the child, drops it: a go that parks is resumed, a timeout fires,
;; and the thread table names only the threads that exist. Without it each check's
;; child hangs (exit 2 below), as the forked agents of a Seon root did (2026-10-22).
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

(check-eq "the parent's carriers are up" (parked-go) true)
(a/<!! (a/timeout 1))
(check-eq "a go that parks is resumed in the child" (in-child parked-go) 0)
(check-eq "a timeout fires in the child" (in-child #(= :t (a/alt!! (a/timeout 10) :t))) 0)
(check-eq "the child's thread table names no carrier of the parent's (the timer and watcher it names are its own, restarted)"
          (in-child #(not-any? (fn [t] (re-find #"jolt-carrier" (.getName ^Thread t))) (keys (Thread/getAllStackTraces))))
          0)

(if (empty? @failures)
  (do (println "FORK-CHILD-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
