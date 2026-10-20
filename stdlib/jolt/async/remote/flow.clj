;; jolt.async.remote.flow — a Flow proc that stands for a process reached only
;; through channels: one in another OS process (jolt.async.remote), or any system
;; with channels of its own. It is an ordinary flow/process step, so its pid takes
;; part in :conns, update!, ping, pause and stop like any other, and Flow's own
;; resolver routes what arrives.
(ns jolt.async.remote.flow
  (:require [clojure.core.async.flow :as flow]))

(defn proc
  "A proc-def (a value for :procs) for a process outside this flow.

    :ins   {in-id ch}   the in-port [pid in-id] IS ch: what the flow sends there is
                        put straight on ch (a remote end's :out). Dropping the
                        in-port (update!) closes ch.
    :outs  {out-id ch}  every value taken from ch (a remote end's :in) is output on
                        [pid out-id] and routed by :conns. When ch closes (the peer
                        ended or died) the flow's report gets
                        {::flow/pid pid ::flow/closed out-id}.
    :sends {id ch}      ch carries [coord msg] pairs: msg is sent to coord, any
                        [pid in-id] in the flow, exactly as a step output keyed by
                        a coord is.

  Pause stops taking from :outs and :sends, so the peer is back-pressured; values
  already put on an :ins channel are the peer's to take. In-port and out-port ids
  must differ, as for any proc."
  [{:keys [ins outs sends]}]
  (let [doc (fn [m d] (zipmap (keys m) (repeat d)))
        desc {:ins (doc ins "a given channel")
              :outs (doc outs "the values taken from a given channel")
              :workload :io}
        readers (merge outs sends)
        sends? (set (keys sends))
        step (fn
               ([] desc)
               ([args] {::flow/in-ports readers
                        ;; the given :ins channels are the peer's to read, not this loop's
                        ::flow/input-filter (set (keys readers))
                        ::pid (::flow/pid args)})
               ([state _] state)
               ([state cid msg]
                (cond
                  (nil? msg) [state {::flow/report [{::flow/pid (::pid state) ::flow/closed cid}]}]
                  (sends? cid) (let [[coord m] msg] [state {coord [m]}])
                  :else [state {cid [msg]}])))]
    {:proc (flow/process step)
     :chan-opts (into {} (map (fn [[k ch]] [k {:chan ch}])) ins)}))
