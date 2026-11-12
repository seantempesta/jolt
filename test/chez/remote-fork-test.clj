;; jolt.async.remote.flow/fork!: a child forked as a proc of a running flow, from a process on its
;; main carrier (one OS thread, as a process that forks agents runs). The child's values reach the
;; proc its out-port is connected to, in order, then its end; the flow's other procs keep running.
;; The child holds only the stream descriptors it is given (0-2 and its own end): a socket the parent
;; holds is closed in it. A child that throws sends the throw as its last value, ::flow/error's
;; shape with the exception as data, and exits 1.
;;
;; Run: bin/jolt run test/chez/remote-fork-test.clj   (prints REMOTE-FORK-TEST OK)
(ns remote-fork-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]
            [jolt.ffi :as ffi]
            [jolt.async.remote :as r]
            [jolt.async.remote.flow :as rflow]))

(ffi/defcfn c-fcntl "fcntl" [:int :int :int] :int)
(ffi/defcfn c-dup2 "dup2" [:int :int] :int)
;; a held stream at a number nothing the child opens reuses (the lowest free number is taken first)
(def held-fd 200)
(defn- open-fd? [fd] (not (neg? (c-fcntl fd 1 0))))   ; F_GETFD

(defn sink ([] {:ins {:in ""} :workload :io}) ([args] args) ([s _] s) ([s _ m] (a/>!! (:to s) m) [s nil]))

(defn- run-child [g pid child]
  (let [to (a/chan 256)]
    (flow/update! g (fn [c] (assoc-in c [:procs (keyword (str (name pid) "-sink"))] {:proc (flow/process #'sink) :args {:to to}})))
    (let [forked (rflow/fork! g pid child {:conns [[[pid :out] [(keyword (str (name pid) "-sink")) :in]]]})
          got (loop [acc []] (let [v (a/<!! to)] (if (and (map? v) (::flow/closed v)) (conj acc v) (recur (conj acc v)))))]
      [forked got])))

(let [r (jolt.host/main-carrier-run!
          (fn []
            (let [g (flow/create-flow {:procs {} :conns []})
                  _ (flow/start g) _ (flow/resume g)
                  [held-a held-b] (#'r/socketpair {})
                  _ (c-dup2 held-a held-fd)
                  [forked got] (run-child g :kid (fn [end] (dotimes [i 100] (a/>!! (:out end) i))
                                                   (a/>!! (:out end) {:held-open? (open-fd? held-fd)})))
                  [thrown-fork thrown] (run-child g :thrower (fn [end] (a/>!! (:out end) :before) (throw (ex-info "boom" {:x 1}))))]
              (flow/stop g)
              {:forked forked :got got :thrown-fork thrown-fork :thrown thrown})))
      {:keys [forked got thrown]} r
      data (butlast got)
      fails (cond-> []
              (not (pos? (:pid forked 0))) (conj [:forked forked])
              (not= (range 100) (take 100 data)) (conj [:order (take 3 data)])
              (not= {:held-open? false} (nth data 100 nil)) (conj [:descriptors "a stream the parent held is open in the child" (nth data 100 nil)])
              (not= {::flow/pid :kid ::flow/closed :out} (last got)) (conj [:end (last got)])
              (not= :before (first thrown)) (conj [:thrower-first (first thrown)])
              (not= "boom" (get-in (second thrown) [::flow/ex :cause])) (conj [:throw-as-value (second thrown)])
              (not= {:x 1} (get-in (second thrown) [::flow/ex :data])) (conj [:throw-data (second thrown)]))]
  (if (empty? fails)
    (println "REMOTE-FORK-TEST OK")
    (do (println "REMOTE-FORK-TEST FAILED" fails) (System/exit 1)))
  (System/exit 0))
