;; A tick budget covers all the work inside its extent, nested bounded runs' included:
;; every budget reads the one count the timer keeps (jolt.host/ticks), so an outer budget
;; ends however much of its work ran inside inner runs, and on a fiber too. Before,
;; inner ticks were never charged to the outer one (Seon's proof run, whose tests each
;; run bounded evals, spun for hours past 2e11 ticks).
(require '[clojure.core.async :as a])
(def failures (atom []))
(defn check [what ok? got] (when-not ok? (swap! failures conj (str what ": got " (pr-str got)))))
(def poll 100000) ;; interrupt-check-ticks: a budget is checked at least this often
(defn run [ticks f] (jolt.host/run-interruptible (jolt.host/make-interrupt) f ticks))
(defn outcome [f] (try (f) :finished (catch Exception e (or (ex-data e) (ex-message e)))))
(defn work [n] (loop [i 0] (if (< i n) (recur (inc i)) i)))
(defn on-fiber [f] (a/<!! (a/go (f))))
(def budget 100000000)
(defn ended-at-budget? [e] (and (map? e) (= budget (:jolt/ticks-budget e)) (<= budget (:jolt/ticks-spent e) (+ budget (* 2 poll)))))

(check "work done inside inner runs ends the outer budget"
       (ended-at-budget? (outcome #(run budget (fn [] (dotimes [_ 50] (run 1000000000 (fn [] (work 20000000)))))))) nil)
(check "an endless loop of inner runs ends at the outer budget"
       (ended-at-budget? (outcome #(run budget (fn [] (loop [] (run 10000000 (fn [] (reduce + (range 100000)))) (recur)))))) nil)
(check "the outer's own work between inner runs is counted"
       (ended-at-budget? (outcome #(run budget (fn [] (dotimes [_ 4000] (work 50000) (run 10000000 (fn [] (+ 1 2)))))))) nil)
(check "an inner budget caught inside the outer does not keep it running"
       (ended-at-budget? (outcome #(run budget (fn [] (loop [] (try (run 10000000 (fn [] (work 1000000000))) (catch Exception _ nil)) (recur)))))) nil)
(let [e (outcome #(run budget (fn [] (run 10000000 (fn [] (work 1000000000))))))]
  (check "the error names the budget that ran out: the inner's" (= 10000000 (:jolt/ticks-budget e)) e))
;; on a fiber (a go block, preempted at its quantum)
(check "on a fiber, 0.95x the budget's work finishes (a tick is counted once)"
       (= :finished (on-fiber #(outcome (fn [] (run budget (fn [] (work 95000000))))))) nil)
(check "on a fiber, a loop of inner runs ends at the outer budget"
       (ended-at-budget? (on-fiber #(outcome (fn [] (run budget (fn [] (loop [] (run 10000000 (fn [] (reduce + (range 100000)))) (recur)))))))) nil)
;; the count itself: what a body ran (jolt.host/ticks-of), budget or none, and its cost
(check "a million iterations count about a million ticks" (< 900000 (jolt.host/ticks-of #(work 1000000)) 1300000) (jolt.host/ticks-of #(work 1000000)))
(check "inside a budget, the same" (< 900000 (run budget #(jolt.host/ticks-of (fn [] (work 1000000)))) 1300000) nil)
(check "an empty body's measure costs a few ticks" (< (jolt.host/ticks-of (fn [] nil)) 100) (jolt.host/ticks-of (fn [] nil)))
;; the law: jolt arms the engine timer in one place, jolt-set-timer! (fibers.ss), so the count
;; is the timer's own; jolt-thread-ticks reads it without re-arming. Any other set-timer in the
;; host would run ticks the count never sees. Exactly one site, so a run that found no file
;; (another directory) is red, not vacuously green.
(let [files (filter #(.endsWith (str %) ".ss") (file-seq (java.io.File. "host/chez")))
      sites (for [f files [i line] (map-indexed vector (clojure.string/split-lines (slurp f)))
                  :let [code (clojure.string/replace line #";.*" "")]
                  :when (re-find #"\(set-timer[ )]" code)]
              [(str f) (inc i)])
      owner (fn [[f n]] (let [lines (clojure.string/split-lines (slurp f))]
                          (some #(second (re-find #"^\(define \(([^ )]+)" %)) (reverse (take n lines)))))]
  (check "jolt-set-timer! is the one site that arms the engine timer"
         (and (= 1 (count sites)) (.endsWith (ffirst sites) "fibers.ss") (= "jolt-set-timer!" (owner (first sites))))
         (map (juxt identity owner) sites)))
;; a park inside nested budgets (a fiber's): its unwind runs on the carrier with no fiber
;; current, and a fiber's budget read against the carrier's count looked spent at once
;; (it escaped from the scheduler mid-switch). Every carrier is warmed far past the budgets.
(run! a/<!! (doall (for [_ (range 64)] (a/go (work 5000000)))))
(let [rs (frequencies (repeatedly 50 #(on-fiber (fn [] (outcome (fn [] (run 2000000 (fn [] (run 1000000 (fn [] (Thread/sleep 1) (work 1000)))))))))))]
  (check "a park inside nested budgets on a fiber resumes and finishes" (= {:finished 50} rs) rs))
;; a measure around a bounded run counts the work after it (the run's exit keeps the
;; measure's arm), and a throw out of a measure leaves the timer disarmed
(let [t-run (jolt.host/ticks-of #(run 1000000000 (fn [] 1)))
      t-work (jolt.host/ticks-of #(work 1000000))
      t-both (jolt.host/ticks-of #(do (run 1000000000 (fn [] 1)) (work 1000000)))]
  (check "ticks-of around a bounded run is the run's and the work's" (<= (abs (- t-both (+ t-run t-work))) 10) [t-run t-work t-both]))
(try (jolt.host/ticks-of #(throw (ex-info "out" {}))) (catch Exception _ nil))
;; nothing armed after it, so the count stands still while idle work runs (a leaked arm
;; would count the million)
(let [t0 (jolt.host/ticks) _ (work 1000000) d (- (jolt.host/ticks) t0)]
  (check "a measure a throw left is closed: the idle count stands still" (< d 100) d))
;; what did not change
(check "a lone budget ends an endless loop" (ended-at-budget? (outcome #(run budget (fn [] (loop [i 0] (recur (inc i))))))) nil)
(check "an inner run that fits returns its value" (= 4950 (run budget (fn [] (run 10000000 (fn [] (reduce + (range 100))))))) nil)
(check "off a fiber, 0.99x the budget's work finishes" (= :finished (outcome #(run budget (fn [] (work 99000000))))) nil)

(if (empty? @failures)
  (do (println "NESTED-BUDGET-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
