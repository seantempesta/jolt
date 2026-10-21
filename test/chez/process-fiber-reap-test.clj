;; A child's exit status read by .waitFor on a FIBER (after its output's EOF, which
;; parks in the io poller) is recorded and answered, and nothing throws. The exit box
;; is written under the process mutex and the pipes are let go of after it: those
;; shutdowns can park, and a fiber must not park holding a counted lock (process.ss,
;; proc-locked-reap). 30 children at once, three rounds.
(require '[clojure.core.async :as a])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(defn run-one [i]
  (let [p (.start (doto (ProcessBuilder. ^java.util.List ["sh" "-c" (str "echo " i "; exit " (mod i 7))])
                    (.redirectErrorStream true)))]
    (a/go (let [out (slurp (.getInputStream p))]
            (try [i (.trim ^String out) (.waitFor p)] (catch Throwable e [i :threw (ex-message e)]))))))

(dotimes [round 3]
  (let [n 30 rs (mapv run-one (range n)) got (mapv a/<!! rs)]
    (check-eq (str "round " round ": every child's output and exit status, read on fibers")
              got (mapv (fn [i] [i (str i) (mod i 7)]) (range n)))))

(if (empty? @failures)
  (do (println "PROCESS-FIBER-REAP-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
