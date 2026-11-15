;; A step runs in a measure labelled [pid in-port] (flow impl, jolt.host/measured), so the fiber dump names
;; the step a fiber is in and the ticks it has run since that step began: a hung step says which it is and how
;; long it has worked (the owner's item 3, 2026-11-15; a fiber's lifetime ticks alone were misread as a step's).
;; Here: (a) a fiber that spins in a labelled measure and then parks shows its label and about its spin in ticks;
;; (b) a Flow consumer whose step spins and then blocks shows "in [:c :in]" in the dump.
;;
;; Run: bin/jolt run test/chez/step-measure-test.clj   (prints STEP-MEASURE-TEST OK)
(ns step-measure-test
  (:require [clojure.core.async :as a] [clojure.core.async.flow :as flow]))

(defn- fail! [& parts] (println (apply str "FAIL step-measure: " parts)) (System/exit 1))

(defn spin [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc i)) acc)))

(defn ticks-in [dump label]
  (when-let [[_ n] (re-find (re-pattern (str "in " (java.util.regex.Pattern/quote label) ", (\\d+) ticks since it began")) dump)]
    (Long/parseLong n)))

;; (a) a labelled measure on a fiber that parks after its spin
(let [never (a/chan)
      spun (jolt.host/ticks-of #(spin 200000))]
  (a/go (jolt.host/measured [:a :in] (fn [] (spin 200000) (a/<!! never))))
  (Thread/sleep 300)
  (let [n (ticks-in (jolt.host/fiber-dump) "[:a :in]")]
    (when-not (and n (> n (quot spun 2)))
      (fail! "the parked fiber's measure is not named with its spin (" spun " ticks): " n))))

;; (b) every Flow step runs in its [pid in-port] measure
(def gate (promise))
(defn producer
  ([] {:ins {:go "start"} :outs {:out "values"} :workload :io})
  ([_] {}) ([s _] s)
  ([s _ _] [s {:out [1]}]))
(defn consumer
  ([] {:ins {:in "values"} :outs {} :workload :io})
  ([_] {}) ([s _] s)
  ([s _ _] (spin 200000) @gate [s {}]))

(let [g (flow/create-flow {:procs {:p {:proc (flow/process #'producer)} :c {:proc (flow/process #'consumer)}}
                           :conns [[[:p :out] [:c :in]]]
                           :stop-timeout-ms 300})]
  (flow/start g)
  (flow/resume g)
  @(flow/inject g [:p :go] [:now])
  (Thread/sleep 300)
  (let [n (ticks-in (jolt.host/fiber-dump) "[:c :in]")]
    (when-not (and n (pos? n)) (fail! "the stuck step is not named in the dump: " n)))
  (deliver gate true)
  (flow/stop g))

(println "STEP-MEASURE-TEST OK")
(System/exit 0)
