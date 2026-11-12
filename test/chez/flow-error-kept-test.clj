;; clojure.core.async.flow's ::flow/error never drops (jolt): a proc that throws 150 times while nobody
;; reads the error chan waits for room, and draining the chan finds all 150, in order (upstream's
;; sliding buffer of 100 kept the last 100).
;;
;; Run: bin/jolt run test/chez/flow-error-kept-test.clj   (prints FLOW-ERROR-KEPT-TEST OK)
(ns flow-error-kept-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]))

(defn thrower ([] {:ins {:in ""} :workload :io}) ([args] args) ([s _] s) ([s _ m] (throw (ex-info "boom" {:n m}))))

(let [g (flow/create-flow {:procs {:t {:proc (flow/process #'thrower)}} :conns []})
      {:keys [error-chan]} (flow/start g)
      n 150]
  (flow/resume g)
  ;; all 150 messages go in before anything is read: the proc waits on the full error chan meanwhile
  (let [feeder (a/thread (dotimes [i n] @(flow/inject g [:t :in] [i])))
        ;; nothing is read until the feeder is done or has waited a second: under a sliding buffer it finishes
        ;; and 50 errors are gone; under the fixed one it waits on the full error chan
        _ (a/alts!! [feeder (a/timeout 1000)])
        got (loop [acc []] (if (= n (count acc)) acc
                             (let [[v _] (a/alts!! [error-chan (a/timeout 5000)])]
                               (if v (recur (conj acc (:n (ex-data (::flow/ex v))))) acc))))]
    (a/<!! feeder)
    (flow/stop g)
    (if (= (range n) got)
      (println "FLOW-ERROR-KEPT-TEST OK")
      (do (println "FLOW-ERROR-KEPT-TEST FAILED" {:count (count got) :first (take 3 got)}) (System/exit 1)))
    (System/exit 0)))
