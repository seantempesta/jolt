;; A ping waits only for the procs that can answer it (flow's find, web-2's at root): a proc mid-step reads control only
;; between steps, so a ping waited its whole timeout whenever any proc was stepping, and at root the asker is itself such
;; a proc (every eval runs as a server-op worker), so stall-state's ping waited 10 s every time, its own worker unanswered.
;; The ping waits until every proc not :stepping when it was sent has answered; a stepping proc is named by its site and
;; listed unanswered, and a late pong from it is merged, never counted.
;;
;; No clock decides here: the ping's timeout is 600 s and the gate's alarm (Makefile, 120 s) ends a run that waits for it.
;; Run: target/release/jolt run test/chez/flow-ping-stepping-test.clj   (prints FLOW-PING-STEPPING-TEST OK)
(ns flow-ping-stepping-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]))

(defn- fail! [& parts] (println (apply str "FAIL flow-ping-stepping: " parts)) (System/exit 1))
(def G (atom nil))
(def held (promise))
(def answer (promise))
(defn asker
  ([] {:ins {:in "asks"} :outs {}})
  ([_] {})
  ([s _] s)
  ([s _ _] (deliver answer (flow/ping @G :timeout-ms 600000)) [s {}]))
(defn blocker
  ([] {:ins {:in "holds"} :outs {}})
  ([_] {})
  ([s _] s)
  ([s _ _] @held [s {}]))
(defn idle
  ([] {:ins {:in "nothing"} :outs {}})
  ([_] {})
  ([s _] s)
  ([s _ _] [s {}]))

(let [g (flow/create-flow {:procs {:a {:proc (flow/process #'asker)} :b {:proc (flow/process #'blocker)} :c {:proc (flow/process #'idle)}}
                           :stop-timeout-ms 300})]
  (reset! G g)
  (flow/start g)
  (flow/resume g)
  @(flow/inject g [:b :in] [:hold])
  ;; :b is mid-step once its site says so (read without asking it)
  (loop [i 0] (when (and (< i 300) (not (:stepping (::flow/site (get (flow/ping g :timeout-ms 1) :b))))) (Thread/sleep 10) (recur (inc i))))
  @(flow/inject g [:a :in] [:ask])
  (let [r @answer
        unanswered (set (::flow/unanswered (meta r)))]
    (when-not (::flow/pid (get r :c)) (fail! "the idle proc's answer is missing: " (pr-str r)))
    (when-not (= #{:a :b} unanswered) (fail! "the stepping procs are not the unanswered ones: " (pr-str unanswered)))
    (when-not (and (:stepping (::flow/site (get r :a))) (:stepping (::flow/site (get r :b))))
      (fail! "the stepping procs are not named :stepping: " (pr-str (select-keys r [:a :b])))))
  (deliver held true)
  (flow/stop g)
  (println "FLOW-PING-STEPPING-TEST OK")
  (System/exit 0))
