;; A flow's ping shows its queues (task clojure.core.async.flow/ping-says-where-a-proc-is-parked; the owner: "pause the
;; whole system and know how data is queued up and is being processed"): for every running proc, answered or not,
;; ::flow/in-fill (each in-port's jolt.host/chan-fill: :count, :capacity, :parked-puts) and ::flow/site, where its loop
;; waits ({:reading #{cid}}, {:writing cid} on its own out-port, {:paused true}) or what it steps ({:stepping cid}).
;; Read by the pinger without asking the proc, so a consumer stuck in its step is seen. Here: a producer emits 30
;; values to a consumer whose step is stuck on its first; the consumer's in-port is full and the producer parked
;; writing to it. Before: ping said only that the consumer did not answer.
;;
;; Run: bin/jolt run test/chez/flow-ping-queues-test.clj   (prints FLOW-PING-QUEUES-TEST OK)
(ns flow-ping-queues-test
  (:require [clojure.core.async.flow :as flow]))

(defn- fail! [& parts] (println (apply str "FAIL flow-ping-queues: " parts)) (System/exit 1))

(def gate (promise))
(defn producer
  ([] {:ins {:go "start"} :outs {:out "values"} :workload :io})
  ([_] {})
  ([s _] s)
  ([s _ _] [s {:out (vec (range 30))}]))
(defn consumer
  ([] {:ins {:in "values"} :outs {} :workload :io})
  ([_] {})
  ([s _] s)
  ([s _ _] @gate [s {}]))

(let [g (flow/create-flow {:procs {:p {:proc (flow/process #'producer)} :c {:proc (flow/process #'consumer)}}
                           :conns [[[:p :out] [:c :in]]]
                           :stop-timeout-ms 300})]
  (flow/start g)
  (flow/resume g)
  @(flow/inject g [:p :go] [:now])
  (Thread/sleep 300)
  (let [p (flow/ping g {:timeout-ms 200})
        c (get p :c) pr (get p :p)
        fill (get-in c [::flow/in-fill :in])]
    (when-not (some #{:c} (::flow/unanswered (meta p))) (fail! "the stuck consumer answered? " (pr-str (meta p))))
    (when-not (= {:stepping :in} (::flow/site c)) (fail! "the consumer's site is not its step on :in: " (pr-str c)))
    (when-not (and fill (pos? (:capacity fill)) (= (:count fill) (:capacity fill)))
      (fail! "the consumer's in-port is not shown full: " (pr-str fill)))
    (when-not (= {:writing :out} (::flow/site pr)) (fail! "the producer is not shown parked writing :out: " (pr-str pr))))
  (deliver gate true)
  (flow/stop g)
  (println "FLOW-PING-QUEUES-TEST OK")
  (System/exit 0))
