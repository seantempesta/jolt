;; A proc stuck in its step never wedges the graph's control (flow-audit-and-plan-2026-11-14, E4/E5): its control
;; commands are put without blocking under the graph lock, so pause, ping and stop still answer; stop ends at its own
;; bound and closes the stuck proc's control, so the proc exits once released instead of spinning on a closed chan.
;; Before: stop with a full control blocked past 3 s against a 300 ms bound (send-control!'s >!! under the lock).
;;
;; Run: bin/jolt run test/chez/flow-stuck-control-test.clj   (prints FLOW-STUCK-CONTROL-TEST OK)
(ns flow-stuck-control-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]))

(defn- fail! [& parts] (println (apply str "FAIL flow-stuck-control: " parts)) (System/exit 1))
(defn- within [ms f] (let [fu (future (f)) r (deref fu ms ::blocked)] (when (= r ::blocked) (fail! "blocked past " ms " ms")) r))

(def gate (promise))
(def stopped (promise))
(defn stuck
  ([] {:ins {:in "x"} :outs {} :workload :io})
  ([_] {})
  ([s t] (when (= t ::flow/stop) (deliver stopped true)) s)
  ([s _ m] @gate [s {}]))

(let [g (flow/create-flow {:procs {:s {:proc (flow/process #'stuck)}} :stop-timeout-ms 300})]
  (flow/start g)
  (flow/resume g)
  @(flow/inject g [:s :in] [1])
  (Thread/sleep 50)
  ;; the step is stuck: 1025 pauses go in without waiting on its full control chan, and nothing throws
  (within 3000 #(dotimes [_ 1025] (flow/pause-proc g :s)))
  ;; ping answers and names the proc that did not
  (let [p (within 3000 #(flow/ping g {:timeout-ms 200}))]
    (when-not (some #{:s} (::flow/unanswered (meta p))) (fail! "ping did not name the stuck proc: " (pr-str (meta p)))))
  ;; stop ends at its bound, not past it
  (let [t0 (System/nanoTime) _ (within 3000 #(flow/stop g)) ms (/ (- (System/nanoTime) t0) 1e6)]
    (when (> ms 2000) (fail! "stop took " ms " ms against a 300 ms bound")))
  ;; released, the proc sees its control closed and runs its stop transition, then exits
  (deliver gate true)
  (when (= ::late (deref stopped 3000 ::late)) (fail! "the released proc never ran its stop transition"))
  ;; update! removing three stuck procs stops them under one deadline, not one bound each (E3b: 913 ms against 300)
  (let [gate3 (promise)
        stuck3 (fn ([] {:ins {:in "x"} :outs {} :workload :io}) ([_] {}) ([s _] s) ([s _ m] @gate3 [s {}]))
        cfg {:procs (into {} (for [k [:a :b :c]] [k {:proc (flow/process stuck3)}])) :conns []}
        g3 (flow/create-flow (assoc cfg :stop-timeout-ms 300))]
    (flow/start g3)
    (flow/resume g3)
    (doseq [k [:a :b :c]] @(flow/inject g3 [k :in] [1]))
    (Thread/sleep 50)
    (let [t0 (System/nanoTime) _ (within 5000 #(flow/update! g3 {:procs {} :conns []})) ms (/ (- (System/nanoTime) t0) 1e6)]
      (when (> ms 700) (fail! "update! removing 3 stuck procs took " ms " ms against one 300 ms bound")))
    (deliver gate3 true)
    (within 5000 #(flow/stop g3)))
  (println "FLOW-STUCK-CONTROL-TEST OK")
  (System/exit 0))
