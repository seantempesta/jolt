;; A step's tick budget (flow's ask, root's go, 2026-11-14): a proc whose describe declares :step-ticks has each step run
;; under jolt.host/run-interruptible with that budget, so a step that spins (a :mixed proc on its own OS thread, Seon's
;; render) ends at its budget instead of holding its proc for good. Past it the step takes the step-error path: the
;; error chan names the pid and the message, the state is kept, and the next message is taken. Both a :mixed and an
;; :io proc; a proc with no :step-ticks is unchanged. Before: the spinning step never ended.
;;
;; Run: bin/jolt run test/chez/flow-step-ticks-test.clj   (prints FLOW-STEP-TICKS-TEST OK)
(ns flow-step-ticks-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]))

(defn- fail! [& parts] (println (apply str "FAIL flow-step-ticks: " parts)) (System/exit 1))
(def done (atom []))
(defn spinner [workload]
  (fn
    ([] {:ins {:in "values"} :outs {} :workload workload :step-ticks 50000000})
    ([_] {:n 0})
    ([s _] s)
    ([s cid m] (cond (= cid ::flow/step-ended) [s {}]   ; a proc that ignores the ended step is unchanged
                     (= m :spin)
               (loop [] (recur))
                     :else (do (swap! done conj [workload m (:n s)]) [(update s :n inc) {}])))))

(doseq [w [:mixed :io]]
  (let [g (flow/create-flow {:procs {:s {:proc (flow/process (spinner w))}} :stop-timeout-ms 300})
        {:keys [error-chan]} (flow/start g)]
    (flow/resume g)
    @(flow/inject g [:s :in] [:spin :after])
    (let [[e _] (a/alts!! [error-chan (a/timeout 30000)])]
      (when-not e (fail! w ": the spinning step did not end at its budget within 30 s"))
      (when-not (and (= :s (::flow/pid e)) (= :spin (::flow/msg e))
                     (= 50000000 (:jolt/ticks-budget (ex-data (::flow/ex e)))))
        (fail! w ": the error does not name the pid, the message and the budget: " (pr-str (dissoc e ::flow/state)))))
    (Thread/sleep 200)
    (when-not (some #{[w :after 0]} @done) (fail! w ": the next message was not taken, with the state kept: " (pr-str @done)))
    (flow/stop g)))
(println "FLOW-STEP-TICKS-TEST OK")
(System/exit 0)
