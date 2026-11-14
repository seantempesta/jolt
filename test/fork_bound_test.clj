;; Every fork's wait ends: (jolt.host/fork-alone ms) answers -3 when no other thread has parked within ms, and
;; (jolt.host/fork-holders) names the threads that held it. A thread in a C call that is not collect-safe never parks,
;; so a fork beside it waited for good (a Seon test child, 2026-11-14, until its runner's stall bound ended it).
;; Here a thread named "holder" runs Scheme and never parks; the fork, bounded at 200 ms, must
;; answer -3 and name it. Estimate: the 200 ms bound and one walk of the thread list.
;; Run: target/release/jolt run test/fork_bound_test.clj
(require '[jolt.scheme :as scm])
(def stop (atom false))
(def started (promise))
;; a thread that never parks: it runs Scheme until told to stop (so it holds every fork) and reaches safe points (so a
;; collection meanwhile still runs: a holder blocked in a plain C call would wedge the collector, and this test with it)
(doto (Thread. (fn [] (deliver started true) (loop [] (when-not @stop (recur)))) "holder") (.setDaemon true) (.start))
@started
(Thread/sleep 50)
(let [t0 (System/nanoTime)
      pid (jolt.host/fork-alone 200)
      ms (/ (- (System/nanoTime) t0) 1e6)
      holders (when (neg? pid) (jolt.host/fork-holders))]
  (when (zero? pid) (System/exit 0))
  (reset! stop true)
  (println "fork-bound:" pid (format "%.0f ms" (double ms)) (pr-str holders))
  (when-not (and (= -3 pid) (< ms 2000) (some #(= "holder" (:name %)) holders))
    (println "FAIL fork-bound: a fork beside a thread in a plain C call did not answer -3 within its bound naming the holder")
    (System/exit 1))
  (println "fork-bound OK")
  (System/exit 0))
