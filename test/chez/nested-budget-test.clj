;; A tick budget covers all the work inside its extent, nested borrows' included
;; (jolt.host/run-interruptible). Before, an inner borrow's ticks were never charged to
;; the enclosing one and the enclosing one lost its partial tick at each inner entry and
;; exit: work done inside inner bounded runs never ended an outer budget (Seon's proof
;; run, whose tests each run bounded evals, spun for hours past 2e11 ticks).
(def failures (atom []))
(defn check [what ok? got] (when-not ok? (swap! failures conj (str what ": got " (pr-str got)))))
(def poll 100000) ;; interrupt-check-ticks: an extent counts its work a tick at a time
(defn run [ticks f] (jolt.host/run-interruptible (jolt.host/make-interrupt) f ticks))
(defn spent-of [f] (try (f) :finished (catch Exception e (or (:jolt/ticks-spent (ex-data e)) (ex-message e)))))
(defn work [n] (loop [i 0] (if (< i n) (recur (inc i)) i)))
(def budget 100000000)

(let [s (spent-of #(run budget (fn [] (dotimes [_ 50] (run 1000000000 (fn [] (work 20000000)))) :finished)))]
  (check "work done inside inner runs ends the outer budget (the inner's limit is what the outer has left)"
         (and (number? s) (<= s (+ budget poll))) s))
(let [s (spent-of #(run budget (fn [] (loop [] (run 10000000 (fn [] (reduce + (range 100000)))) (recur)))))]
  (check "an endless loop of inner runs ends at the outer budget" (and (number? s) (<= budget s (+ budget (* 2 poll)))) s))
(let [s (spent-of #(run budget (fn [] (dotimes [_ 4000] (work 50000) (run 10000000 (fn [] (+ 1 2)))) :finished)))]
  (check "the outer's own work between inner runs is counted" (and (number? s) (<= budget s (+ budget (* 2 poll)))) s))
(let [s (spent-of #(run budget (fn [] (loop [] (try (run 10000000 (fn [] (work 1000000000))) (catch Exception _ nil)) (recur)))))]
  (check "an inner budget caught inside the outer does not keep it running" (and (number? s) (<= s (+ budget (* 2 poll)))) s))
;; what did not change
(check "a lone budget ends an endless loop" (number? (spent-of #(run budget (fn [] (loop [i 0] (recur (inc i))))))) nil)
(check "an inner run that fits returns its value" (= 4950 (run budget (fn [] (run 10000000 (fn [] (reduce + (range 100))))))) nil)

(if (empty? @failures)
  (do (println "NESTED-BUDGET-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
