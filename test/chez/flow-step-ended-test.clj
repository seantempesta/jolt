;; A step that ends at its :step-ticks budget is handed back to its proc (flow's ask, gating jolt/flow; web-2's review: a
;; render proc answering edge asks left its asker unanswered when a step ended at its budget, a hang). On a budget end
;; only, the proc's transform is called with cid ::flow/step-ended and {:msg :cid :ticks-spent :ticks-budget}, its state
;; kept, and its outputs are sent: the proc answers its asker with a typed error, never silence. Before: the message was
;; dropped and only the error chan heard it.
;;
;; Run: bin/jolt run test/chez/flow-step-ended-test.clj   (prints FLOW-STEP-ENDED-TEST OK)
(ns flow-step-ended-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]))

(defn- fail! [& parts] (println (apply str "FAIL flow-step-ended: " parts)) (System/exit 1))
(def answers (atom []))
(defn server
  ([] {:ins {:req "asks"} :outs {:out "answers"} :workload :mixed :step-ticks 50000000})
  ([_] {})
  ([s _] s)
  ([s cid m]
   (cond (= cid ::flow/step-ended) [s {:out [{:error :budget :asked (:msg m) :ticks-budget (:ticks-budget m)}]}]
         (= m :spin) (loop [] (recur))
         :else [s {:out [{:answer m}]}])))
(defn sink
  ([] {:ins {:in "answers"} :outs {}})
  ([_] {})
  ([s _] s)
  ([s _ m] (swap! answers conj m) [s {}]))

(let [g (flow/create-flow {:procs {:s {:proc (flow/process #'server)} :k {:proc (flow/process #'sink)}}
                           :conns [[[:s :out] [:k :in]]]
                           :stop-timeout-ms 300})]
  (flow/start g)
  (flow/resume g)
  @(flow/inject g [:s :req] [:spin :after])
  (loop [i 0] (when (and (< i 300) (< (count @answers) 2)) (Thread/sleep 100) (recur (inc i))))
  (let [[a b] @answers]
    (when-not (= {:error :budget :asked :spin :ticks-budget 50000000} a)
      (fail! "the ask that ended at its budget was not answered typed: " (pr-str @answers)))
    (when-not (= {:answer :after} b) (fail! "the next ask was not answered: " (pr-str @answers))))
  (flow/stop g)
  (println "FLOW-STEP-ENDED-TEST OK")
  (System/exit 0))
