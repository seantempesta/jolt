;; jolt.async.remote.flow/fork!: a child forked as a proc of a running flow, from a process on its
;; main carrier (one OS thread, as a process that forks agents runs). The child's values reach the
;; proc its out-port is connected to, in order, then its end; the flow's other procs keep running.
;;
;; Run: bin/jolt run test/chez/remote-fork-test.clj   (prints REMOTE-FORK-TEST OK)
(ns remote-fork-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]
            [jolt.async.remote.flow :as rflow]))

(defn sink ([] {:ins {:in ""} :workload :io}) ([args] args) ([s _] s) ([s _ m] (a/>!! (:to s) m) [s nil]))

(let [r (jolt.host/main-carrier-run!
          (fn []
            (let [to (a/chan 256)
                  g (flow/create-flow {:procs {:sink {:proc (flow/process #'sink) :args {:to to}}} :conns []})
                  _ (flow/start g)
                  _ (flow/resume g)
                  forked (rflow/fork! g :kid (fn [end] (dotimes [i 100] (a/>!! (:out end) i)))
                                      {:conns [[[:kid :out] [:sink :in]]]})
                  got (loop [acc []] (let [v (a/<!! to)] (if (map? v) (conj acc v) (recur (conj acc v)))))
                  sink-alive (some? (flow/ping-proc g :sink))]
              (flow/stop g)
              {:forked forked :got got :sink-alive sink-alive})))
      {:keys [forked got sink-alive]} r]
  (if (and (pos? (:pid forked 0))
           (= (range 100) (butlast got))
           (= {::flow/pid :kid ::flow/closed :out} (last got))
           sink-alive)
    (println "REMOTE-FORK-TEST OK")
    (do (println "REMOTE-FORK-TEST FAILED" {:forked forked :n (count got) :last (take-last 2 got) :sink-alive sink-alive})
        (System/exit 1)))
  (System/exit 0))
