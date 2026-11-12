;; clojure.core.async.flow's ::flow/report may drop (a sliding buffer of 100, as upstream), but never silently
;; (jolt): ping's answer carries the count as metadata, ::flow/report-dropped. A proc reports 150 times while nobody
;; reads the report chan: ping shows 50 dropped.
;;
;; Run: bin/jolt run test/chez/flow-report-dropped-test.clj   (prints FLOW-REPORT-DROPPED-TEST OK)
(ns flow-report-dropped-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]))

(defn reporter ([] {:ins {:in ""} :workload :io}) ([args] args) ([s _] s)
  ([s _ n] [s {::flow/report (vec (for [i (range n)] {:i i}))}]))

(let [g (flow/create-flow {:procs {:r {:proc (flow/process #'reporter)}} :conns []})]
  (flow/start g) (flow/resume g)
  @(flow/inject g [:r :in] [150])
  ;; inject answers once the message is queued, not stepped: ping until the step's reports are all sent (20 pings bound it)
  (let [dropped (loop [i 0] (let [d (::flow/report-dropped (meta (flow/ping g)))] (if (or (= 50 d) (= i 20)) d (recur (inc i)))))]
    (flow/stop g)
    (if (= 50 dropped)
      (println "FLOW-REPORT-DROPPED-TEST OK")
      (do (println "FLOW-REPORT-DROPPED-TEST FAILED" {:dropped dropped}) (System/exit 1)))
    (System/exit 0)))
