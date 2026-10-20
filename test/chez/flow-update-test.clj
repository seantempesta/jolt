;; clojure.core.async.flow/update! gate (jolt, lane F2): a running flow changed in place.
;;
;; Run: bin/jolt run test/chez/flow-update-test.clj   (prints timings, then FLOW-UPDATE-TEST OK)
(ns flow-update-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]))

(def failures (atom []))
(defmacro check [label ok? & [detail]]
  `(do (print (str "  .. " ~label "\n")) (flush)
       (let [ok# (try ~ok? (catch Throwable t# (swap! failures conj (str ~label ": threw " (ex-message t#))) ::threw))]
         (when-not (or (true? ok#) (= ::threw ok#))
           (swap! failures conj (str ~label ": " (pr-str ~detail)))))))

(defn await-until [cap-ms pred]
  (let [deadline (+ (System/nanoTime) (* cap-ms 1000000))]
    (loop [] (or (pred) (if (>= (System/nanoTime) deadline) (pred) (do (Thread/sleep 2) (recur)))))))

;; a source fed from outside (an external in-port), a relay, and a sink that counts in order
(defn src ([] {:outs {:out ""} :workload :io}) ([a] (assoc a ::flow/in-ports {:in (:ch a)})) ([s _] s) ([s _ m] [s {:out [m]}]))
(defn relay ([] {:ins {:in ""} :outs {:out ""} :workload :io}) ([a] a) ([s _] s) ([s _ m] [s {:out [m]}]))
(defn sink ([] {:ins {:in ""} :workload :io}) ([a] a) ([s _] s)
  ([s _ m] (swap! (:seen s) conj m) [s nil]))
;; a step whose behavior comes from a var, to show hot reload
(defn tag-fn [m] [:v1 m])
(defn tagger ([] {:ins {:in ""} :outs {:out ""} :workload :io}) ([a] a) ([s _] s) ([s _ m] [s {:out [(tag-fn m)]}]))

(defn base-config [ch seen]
  {:procs {:src {:proc (flow/process #'src) :args {:ch ch}}
           :relay {:proc (flow/process #'relay)}
           :sink {:proc (flow/process #'sink) :args {:seen seen}}}
   :conns [[[:src :out] [:relay :in]] [[:relay :out] [:sink :in]]]})

(defn with-agents [cfg n]
  (-> cfg
      (update :procs into (for [i (range n)] [(keyword (str "a" i)) {:proc (flow/process #'relay)}]))
      (update :conns into (for [i (range n)] [[(keyword (str "a" i)) :out] [:sink :in]]))))

;; 1. under load: add and remove 100 procs, 5 times, while 20,000 messages flow src -> relay
;;    -> sink. Nothing on the unchanged path is lost or reordered.
(let [ch (a/chan 1024) seen (atom [])
      cfg (base-config ch seen)
      g (flow/create-flow cfg)
      _ (flow/start g) _ (flow/resume g)
      feeder (future (dotimes [i 20000] (a/>!! ch i)))
      reports (vec (for [_ (range 5)]
                     [(flow/update! g (with-agents cfg 100)) (flow/update! g cfg)]))
      _ @feeder
      done (await-until 20000 #(= 20000 (count @seen)))]
  (check "add/remove 100 procs x5 under load: all 20,000 arrive, in order"
         (and done (= (range 20000) @seen))
         {:count (count @seen)})
  (check "the unchanged procs were never restarted"
         (every? (fn [[up down]] (and (= 3 (:kept up)) (= 100 (count (:started up))) (= 100 (count (:stopped down))))) reports)
         (mapv (fn [[up down]] (select-keys up [:kept :started])) reports))
  (println "   update! adding 100 procs, ms:" (mapv (comp :ms first) reports) " removing 100:" (mapv (comp :ms second) reports))
  (check "stop answers true" (true? (flow/stop g))))

;; 2. change one conn: src now feeds the sink directly; only the relay's in-chan (its
;;    queued messages) can be lost, and the report says how many.
(let [ch (a/chan 1024) seen (atom [])
      cfg (base-config ch seen)
      g (flow/create-flow cfg)
      _ (flow/start g) _ (flow/resume g)
      _ (dotimes [i 1000] (a/>!! ch i))
      _ (await-until 5000 #(= 1000 (count @seen)))
      rewired (assoc cfg :procs (dissoc (:procs cfg) :relay) :conns [[[:src :out] [:sink :in]]])
      r (flow/update! g rewired)
      _ (dotimes [i 1000] (a/>!! ch (+ 1000 i)))
      done (await-until 5000 #(= 2000 (count @seen)))]
  (check "a rewired conn: every message arrives (nothing was queued on the dropped path)"
         (and done (= (range 2000) @seen)) {:count (count @seen) :report r})
  (check "the report names the stopped proc and the losses" (and (= #{:relay} (:stopped r)) (map? (:lost r))) r)
  (flow/stop g))

;; 3. hot reload: redefining the step's var changes the running proc, no update!
(let [ch (a/chan 16) seen (atom [])
      g (flow/create-flow {:procs {:src {:proc (flow/process #'src) :args {:ch ch}}
                                   :tag {:proc (flow/process #'tagger)}
                                   :sink {:proc (flow/process #'sink) :args {:seen seen}}}
                           :conns [[[:src :out] [:tag :in]] [[:tag :out] [:sink :in]]]})
      _ (flow/start g) _ (flow/resume g)
      _ (a/>!! ch 1)
      _ (await-until 5000 #(= 1 (count @seen)))
      _ (alter-var-root #'tag-fn (constantly (fn [m] [:v2 m])))
      _ (a/>!! ch 2)
      _ (await-until 5000 #(= 2 (count @seen)))]
  (check "a var redefinition is live in the running proc" (= [[:v1 1] [:v2 2]] @seen) @seen)
  (flow/stop g))

;; 4. cost: update! at 1,000 procs (add one / remove one), and per-message throughput
(let [ch (a/chan 1024) seen (atom [])
      cfg (with-agents (base-config ch seen) 1000)
      g (flow/create-flow cfg)
      t0 (System/nanoTime) _ (flow/start g) _ (flow/resume g)
      start-ms (/ (- (System/nanoTime) t0) 1e6)
      ;; the same definition values plus one proc (as a spawn would build it)
      plus (-> cfg (assoc-in [:procs :extra] {:proc (flow/process #'relay)}) (update :conns conj [[:extra :out] [:sink :in]]))
      add-ms (vec (for [_ (range 5)] (let [r (flow/update! g plus)] (flow/update! g cfg) (:ms r))))
      t1 (System/nanoTime)
      _ (dotimes [i 20000] (a/>!! ch i))
      _ (await-until 30000 #(= 20000 (count @seen)))
      msg-us (/ (- (System/nanoTime) t1) 1e3 20000)]
  (println "   1,000 procs: start" (int start-ms) "ms; update! adding one proc, ms:" add-ms "; per message src->relay->sink" msg-us "us")
  (check "1,000-proc flow stops" (true? (flow/stop g))))

(if (empty? @failures)
  (do (println "FLOW-UPDATE-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL" f)) (flush) (System/exit 1)))
