;; A thread that waits to enter a monitor in a main-carrier process waits, and does no work while
;; it does (host/chez/locks.ss jolt-asleep-while, jolt-asleep-notify!). The wait of a thread in a
;; value or monitor wait is counted asleep, and when that makes every thread asleep it wakes the
;; main loop (fibers.ss jolt-asleep-wake-detector!), whose wake is jolt.io-poller's
;; `(locking pm (pipe-write!))`. A thread waiting for pm's monitor made that wake itself, from
;; inside its own wait (bk held): the wake needed pm, waited for it, and the wait made the wake
;; again, one nested call deeper each time (10 stack frames and ~1 KB a pass, ~470,000 passes a
;; second, until the heap ceiling; a wakeup the innermost pass consumed was gone for the ones
;; beneath, which then waited for good). A Seon agent reading a child's two output pipes with two
;; futures did exactly this at the child's exit (2026-10-29: ~1.5 GB of garbage in 3.5 s, then
;; out of memory or a hang). Here a future holds the poller's monitor while a second one waits to
;; enter it; the work done is bytes allocated while it waits, never time, and the waiter must get
;; the monitor once the holder leaves it.
(require '[clojure.core.async :as a] '[jolt.scheme :as scm] '[jolt.async.remote :as remote])

(def nap (scm/eval-string "(let ([nap (foreign-procedure __collect_safe \"usleep\" (unsigned) int)]) (lambda (us) (nap us) #t))"))
(def bytes-total (scm/eval-string "(lambda () (sstats-bytes (statistics)))"))

(def failures (atom []))
(defn check [what ok? detail] (when-not ok? (swap! failures conj (str what ": " (pr-str detail)))))

(jolt.host/main-carrier-run!
  (fn []
    ;; the poller starts at the first fiber wait on a descriptor, and sets the main loop's wake
    (let [[fa fb] (remote/socketpair) ea (remote/open fa {:pump :fiber}) eb (remote/open fb {:pump :fiber})]
      (a/>!! (:out ea) 7)
      (a/<!! (:in eb))
      (remote/close! ea) (remote/close! eb))
    (let [pm @#'jolt.io-poller/pm
          held (promise)
          holder (future (locking pm (deliver held true) (nap 300000) :held))
          _ @held
          b0 (bytes-total)
          waiter (future (locking pm :entered))
          got (deref waiter 10000 ::never)
          used (- (bytes-total) b0)]
      (check "the waiter entered the monitor once the holder left it" (= :entered got) got)
      (check "the holder left it" (= :held @holder) nil)
      ;; the waiter slept for the nap, it did not run, it did not run (before: ~100 MB in 0.3 s)
      (println "the waiter allocated" used "bytes in a 0.3 s wait")
      (check "the wait allocates little (bytes)" (< used (* 8 1024 1024)) used))))

(if (empty? @failures)
  (do (println "MAIN-CARRIER-MONITOR-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
