;; A process whose main carrier's program returns starts its threads then (fibers.ss
;; jolt-main-carrier-leave!), the all-asleep detector among them, as it starts its carriers: a
;; count of its threads by name finds exactly one "jolt-asleep" from the return. Unfixed, the
;; detector started only when the asleep hint first fired (locks.ss jolt-asleep-all-hint?), which
;; follows timing: here one carrier and three busy threads keep the hint from firing, so it was
;; absent, and Seon's root, counting its threads against what it declares, found it in some runs
;; and not in others.
(require '[clojure.core.async :as a])

(alter-var-root #'a/*fiber-carrier-count* (constantly 1))
(def stop (atom false))
(def busy (doall (for [_ (range 3)] (future (loop [n 0] (if @stop n (recur (inc n))))))))

(defn named [s] (count (filter #(= s (.getName ^Thread %)) (keys (Thread/getAllStackTraces)))))

(def before (jolt.host/main-carrier-run! (fn [] (named "jolt-asleep"))))
(def after (named "jolt-asleep"))
(reset! stop true)
(run! deref busy)

(if (= [0 1] [before after])
  (println "main-carrier-leaves-its-detector-test OK")
  (do (println "FAIL: jolt-asleep threads [during the program, after its return]: want [0 1], got" [before after])
      (System/exit 1)))
