;; ::flow/drain (jolt's, beside pause, resume and stop): a process told to drain keeps reading its in-ports as when
;; running, so what is queued is finished; its transition arity runs once with ::flow/drain; it ends on stop. The
;; owner's switch-over model (drain, clean shutdown, the same db). Shape of flow's lab e24: one message stepping, nine
;; queued behind it. Stopped, the nine are lost; drained, all ten are finished, then the stop ends it.
;;
;; Run: bin/jolt run test/chez/flow-drain-test.clj   (prints FLOW-DRAIN-TEST OK)
(ns flow-drain-test
  (:require [clojure.core.async :as a] [clojure.core.async.flow :as flow]))

(defn- fail! [& parts] (println (apply str "FAIL flow-drain: " parts)) (System/exit 1))


(defn run
  "A consumer whose first step waits on a gate while nine more messages queue behind it; end-with (fn [g]) ends the flow
  once the gate opens. Answers [steps transitions]."
  [end-with]
  (let [gate (promise) steps (atom 0) transitions (atom []) all (promise)
        consumer (fn
                   ([] {:ins {:in "values"} :outs {} :workload :io})
                   ([_] {})
                   ([s t] (swap! transitions conj t) s)
                   ([s _ _] (when (= 1 (swap! steps inc)) @gate) (when (= 10 @steps) (deliver all true)) [s {}]))
        g (flow/create-flow {:procs {:c {:proc (flow/process consumer)}} :stop-timeout-ms 2000})]
    (flow/start g)
    (flow/resume g)
    @(flow/inject g [:c :in] (vec (range 10)))
    (Thread/sleep 100)
    (end-with g gate all)
    [@steps @transitions]))

;; stopped while one steps and nine queue: the stop ends it after the step in hand
(let [[steps _] (run (fn [g gate _] (future (Thread/sleep 100) (deliver gate true)) (flow/stop g)))]
  (when-not (< steps 10) (fail! "a stop finished every queued message (" steps "): nothing for drain to show")))

;; drained: every queued message is finished, then the stop ends it
(let [[steps transitions] (run (fn [g gate all]
                                 (flow/drain g)
                                 (deliver gate true)
                                 (when-not (deref all 5000 nil) (fail! "a drained process did not finish its queue"))
                                 (flow/stop g)))]
  (when-not (= 10 steps) (fail! "a drained process finished " steps " of 10"))
  (when-not (= 1 (count (filter #{::flow/drain} transitions)))
    (fail! "its transition ran with ::flow/drain " (count (filter #{::flow/drain} transitions)) " times: " transitions)))

(println "FLOW-DRAIN-TEST OK")
(System/exit 0)
