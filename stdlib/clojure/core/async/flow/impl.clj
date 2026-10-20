;;   Copyright (c) Rich Hickey and contributors. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.async.flow.impl
  (:require [clojure.core.async :as async]
            [clojure.core.async.flow :as-alias flow]
            [clojure.core.async.flow.spi :as spi]
            [clojure.core.async.flow.impl.graph :as graph]
            [clojure.core.async.impl.dispatch :as disp]
            [clojure.walk :as walk]
            [clojure.datafy :as datafy])
  (:import [java.util.concurrent Future Executor TimeUnit FutureTask]
           [java.util.concurrent.locks ReentrantLock]))

(set! *warn-on-reflection* true)

(defn datafy [x]
  (condp instance? x
    clojure.lang.Fn (-> x str symbol)
    Executor (str x)
    clojure.lang.Var (symbol x)
    (datafy/datafy x)))

;; jolt: a go block runs on an OS thread unless *go-backend* is :fiber. Flow's own
;; plumbing (the control mult, fan-out mults, ping's take) is nothing but channel ops,
;; which park on a fiber, so it runs on fibers: a live graph then costs no OS thread
;; of its own (1,000 live graphs: 21 OS threads, against 1,021). A proc's own code is
;; untouched: it runs on the executor its workload names.
(defmacro ^:private fiber-go [& body]
  `(binding [async/*go-backend* :fiber] ~@body))

(defn futurize [f {:keys [exec]}]
  (fn [& args]
    (let [^Executor e (if (instance? Executor exec)
                        exec
                        (disp/executor-for exec))
          fut (FutureTask. #(apply f args))]
      (.execute e fut)
      fut)))

(defn prep-proc [ret pid {:keys [proc, args, chan-opts] :or {chan-opts {}}}]
  (let [{:keys [ins outs signal-select]} (spi/describe proc)
        copts (fn [cs]
                (zipmap (keys cs) (map #(chan-opts %) (keys cs))))
        inopts (copts ins)
        outopts (copts outs)]
    (when (or (some (partial contains? inopts) (keys outopts))
              (some (partial contains? outopts) (keys inopts)))
      (throw (ex-info ":ins and :outs cannot share ids within a process"
                      {:pid pid :ins (keys inopts) :outs (keys outopts)})))
    (assoc ret pid {:pid pid :proc proc :ins inopts :outs outopts :args args :signal-select signal-select})))

;; ---- jolt: a graph that update! can change in place (lane F2, after V1's JVM design,
;; notes/option3-V1-jvm-flow-update.md §3 and §5) -------------------------------------
;;
;; 1. A proc's data outputs are not handed to it at start: it gets only ::flow/error and
;;    ::flow/report in :outs, so every data output falls through send-outputs to
;;    spi/get-write-chan on a resolver of its own (one per pid: send-outputs passes the
;;    bare cid). The resolver reads the graph's route table at send time, so rewiring an
;;    output is a table swap, never a producer restart.
;; 2. A route is nil (an unconnected output, dropped), the consumer's in-chan (1:1), or a
;;    mult's source (fan-out). Taps are close? false. A mult, once made, stays for its
;;    output's life (collapsing it to a direct put could reorder that producer's messages).
;; 3. No control mult: each proc has its own control chan, and every command is put on its
;;    targets' chans under the graph lock, in issue order. A dead proc's full chan cannot
;;    wedge anyone else's commands; a per-proc stop is O(1).
;; 4. update! diffs proc fingerprints (step, describe without fns, args, chan-opts):
;;    unchanged procs keep their loop, state, in-chans and queued messages; removed and
;;    changed procs are stopped and their exit awaited (one never starts twice); in-chans
;;    survive by coord and options, a dropped one is closed and drained, its losses reported.
;; 5. stop sends stop to every proc and awaits every exit (bounded), as before.

(defn- describe-data [desc] (into {} (remove (comp fn? val)) desc))

(defn- proc-plan
  "One proc's plan: its launcher, describe, in/out coords with their chan opts, fingerprint."
  [pid {:keys [proc args chan-opts] :or {chan-opts {}}}]
  (let [desc (spi/describe proc)
        step (:step (datafy/datafy proc))
        coords (fn [k] (into {} (map (fn [cid] [[pid cid] (get chan-opts cid)])) (keys (k desc))))]
    {:pid pid :proc proc :args args :desc desc :signal-select (:signal-select desc)
     :ins (coords :ins) :outs (coords :outs)
     :fingerprint [(or step proc) (describe-data desc) args chan-opts]}))

(defn- conn-map [plans conns]
  (let [ins (into {} (mapcat :ins) (vals plans))
        outs (into {} (mapcat :outs) (vals plans))]
    (reduce (fn [m [out in :as c]]
              (if (and (contains? outs out) (contains? ins in))
                (update m out (fnil conj #{}) in)
                (throw (ex-info "invalid connection" {:conn c}))))
            {} conns)))

(defn- set-diff [a b] (into #{} (remove b) a))

(defn- plan-diff
  "What an update must do, as data: pids to stop, start, keep; in-chans to drop and make."
  [old-plans new-plans]
  (let [old-pids (set (keys old-plans)) new-pids (set (keys new-plans))
        both (into #{} (filter old-pids) new-pids)
        kept (into #{} (filter #(= (:fingerprint (old-plans %)) (:fingerprint (new-plans %)))) both)
        changed (set-diff both kept)
        old-ins (into {} (mapcat :ins) (vals old-plans))
        new-ins (into {} (mapcat :ins) (vals new-plans))
        same-in? (fn [coord] (and (contains? old-ins coord) (contains? new-ins coord) (= (old-ins coord) (new-ins coord))))]
    {:stop (into (set-diff old-pids new-pids) changed)
     :start (into (set-diff new-pids old-pids) changed)
     :kept kept
     :drop-ins (into #{} (remove same-in?) (keys old-ins))
     :make-ins (into #{} (remove same-in?) (keys new-ins))}))

(defn- make-chan [error-chan [[pid cid] {:keys [buf-or-n xform]}]]
  (if xform
    (async/chan buf-or-n xform
                (fn [ex] (async/put! error-chan #::flow{:ex ex :pid pid :cid cid :xform xform}) nil))
    (async/chan (or buf-or-n 10))))

(defn- drain!
  "Close c and take what is left in it (buffered, or parked puts); answers how many."
  [c]
  (async/close! c)
  (loop [n 0] (if (some? (async/poll! c)) (recur (inc n)) n)))

(defn- routes-for
  "out coord -> route, given the in-chans, the conn map and the mults that exist.
  Answers [routes mults], with a mult for every new fan-out (or self-feeding output)."
  [in-chans cmap mults]
  (reduce-kv
    (fn [[rs ms] out ins]
      (if (or (contains? ms out) (< 1 (count ins)) (= (first out) (ffirst ins)))
        (let [{:keys [src] :as m} (or (ms out)
                                      (let [src (async/chan 10)] {:src src :mult (fiber-go (async/mult src)) :taps {}}))]
          [(assoc rs out src) (assoc ms out m)])
        [(assoc rs out (in-chans (first ins))) ms]))
    [{} mults] cmap))

(defn- retap!
  "Make each mult's taps exactly its conns' current in-chans (by identity)."
  [mults cmap in-chans]
  (reduce-kv (fn [ms out {:keys [mult taps] :as m}]
               (let [want (into {} (map (fn [in] [in (in-chans in)])) (get cmap out #{}))]
                 (doseq [[in c] taps :when (not (identical? c (want in)))] (async/untap mult c))
                 (doseq [[in c] want :when (not (identical? c (taps in)))] (async/tap mult c false))
                 (assoc ms out (assoc m :taps want))))
             {} mults))

(defn- send-control!
  "Put cmd on each pid's own control chan, in order (called under the graph lock)."
  [rt pids cmd]
  (doseq [pid pids]
    (when-let [c (get-in @rt [:running pid :control])]
      (async/>!! c (assoc cmd ::flow/to pid)))))

(defn- proc-resolver [rt pid execs]
  (reify spi/Resolver
    (get-write-chan [_ coord]
      (let [{:keys [routes in-chans plans]} @rt]
        (if (vector? coord)
          (if-let [[_ c] (or (find routes coord) (find in-chans coord))]
            c
            (throw (ex-info "can't resolve channel with io-id" {:io-id coord})))
          ;; the bare cid send-outputs passes for an output not in :outs
          (let [k [pid coord]]
            (if-let [[_ c] (find routes k)]
              c
              ;; nil drops: an unconnected output, or a proc no longer in the plan
              (when (and (contains? plans pid) (not (contains? (get-in plans [pid :outs]) k)))
                (throw (ex-info "can't resolve channel with io-id" {:io-id k}))))))))
    (get-exec [_ context] (or (execs context) (disp/executor-for context)))))

(defn- start-proc! [rt execs {:keys [pid proc args ins]}]
  (let [{:keys [in-chans error report castees cast status]} @rt
        control (async/chan 10)
        fut (spi/start proc {:pid pid :args (assoc args ::flow/pid pid)
                             :resolver (proc-resolver rt pid execs) :cast cast
                             :ins (assoc (into {} (map (fn [[[_ cid :as k] _]] [cid (in-chans k)])) ins)
                                         ::flow/control control
                                         ::flow/casts (get-in castees [pid :chan]))
                             :outs {::flow/error error ::flow/report report}})]
    (swap! rt assoc-in [:running pid] {:future fut :control control})
    (when (= :running status)
      (async/>!! control #::flow{:command ::flow/resume :to pid}))))

(defn- await-exits
  "Await each future up to the deadline; answers the pids still running."
  [running stop-timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) stop-timeout-ms)]
    (into #{} (keep (fn [[pid {:keys [^Future future]}]]
                      (when-not (try (.get future (max 1 (- deadline (System/currentTimeMillis))) TimeUnit/MILLISECONDS) true
                                     (catch Throwable _ (.isDone future)))
                        pid)))
          running)))

(defn- stop-proc!
  "Stop one proc and await its exit. Answers nil, or the pid when it missed the bound."
  [rt pid stop-timeout-ms]
  (when-let [{:keys [control] :as r} (get-in @rt [:running pid])]
    (async/>!! control #::flow{:command ::flow/stop :to pid})
    (if (seq (await-exits {pid r} stop-timeout-ms))
      pid
      (do (async/close! control) (swap! rt update :running dissoc pid) nil))))

(defn create-flow
  "see lib ns for docs (jolt: update! changes a running flow in place, above)"
  [{:keys [procs conns mixed-exec io-exec compute-exec stop-timeout-ms] :or {stop-timeout-ms 10000} :as definition}]
  (let [lock (ReentrantLock.)
        execs {:mixed mixed-exec :io io-exec :compute compute-exec}
        _ (assert (every? #(or (nil? %) (instance? Executor %)) (vals execs))
                  "mixed-exe, io-exec and compute-exec must be Executors")
        ;; the definition update! last applied (start starts from it)
        current (atom (select-keys definition [:procs :conns]))
        _ (conn-map (into {} (map (fn [[pid d]] [pid (proc-plan pid d)])) procs) conns)
        rt (atom nil)
        locked (fn [f] (.lock lock) (try (f) (finally (.unlock lock))))
        apply! (fn [{:keys [procs conns]}]
                 (let [t0 (System/nanoTime)
                       old @rt
                       ;; a proc whose definition is the identical value keeps its plan
                       plans (into {} (map (fn [[pid d]] [pid (if (identical? d (get-in old [:defs pid])) (get-in old [:plans pid]) (proc-plan pid d))]))
                                   procs)
                       cmap (conn-map plans conns)
                       {:keys [stop start drop-ins make-ins kept]} (plan-diff (:plans old) plans)
                       new-ins (into {} (mapcat :ins) (vals plans))
                       ;; 1. the new in-chans (and castees)
                       in-chans (-> (apply dissoc (:in-chans old) drop-ins)
                                    (into (map (fn [k] [k (make-chan (:error old) [k (new-ins k)])])) make-ins))
                       castees (-> (apply dissoc (:castees old) stop)
                                   (into (map (fn [pid] [pid {:select (:signal-select (plans pid))
                                                              :chan (async/chan (async/sliding-buffer 100))}]))
                                         start))
                       ;; 2. routes and mults: a producer's next send goes the new way
                       new-outs (into {} (mapcat :outs) (vals plans))
                       live-mults (into {} (filter (comp #(contains? new-outs %) key)) (:mults old))
                       [rts mults] (routes-for in-chans cmap live-mults)
                       mults (retap! mults cmap in-chans)]
                   (swap! rt assoc :plans plans :defs procs :in-chans in-chans :castees castees :routes rts :mults mults)
                   ;; 3. stop removed and changed procs, awaiting each exit; one that missed
                   ;; its bound is never started twice (every later update retries it)
                   (let [pending (:pending-exit old #{})
                         missed (into #{} (keep #(stop-proc! rt % stop-timeout-ms)) (into pending stop))
                         start (-> start (into (filter #(contains? plans %)) (remove missed pending)) (set-diff missed))
                         _ (swap! rt assoc :pending-exit missed)
                         ;; 4. a removed output's mult closes (its taps are close? false)
                         _ (doseq [[out m] (:mults old) :when (not (contains? mults out))]
                             (async/untap-all (:mult m)) (async/close! (:src m)))
                         ;; 5. dropped in-chans: closed and drained, losses counted
                         lost (into {} (map (fn [k] [k (drain! (get-in old [:in-chans k]))])) drop-ins)
                         ;; 6. start added and changed procs
                         _ (doseq [pid start] (start-proc! rt execs (plans pid)))]
                     {:stopped stop :started start :kept (count kept)
                      :lost (into {} (filter (comp pos? val)) lost)
                      :missed-exit missed
                      :ms (/ (- (System/nanoTime) t0) 1e6)})))]
    (reify
      clojure.core.protocols/Datafiable
      (datafy [_]
        (walk/postwalk datafy (assoc @current :execs execs :chans (select-keys @rt [:in-chans :routes :error :report]))))

      clojure.core.async.flow.impl.graph.Graph
      (start [_]
        (locked
          (fn []
            (if-let [{:keys [report error]} @rt]
              {:report-chan report :error-chan error :already-running true}
              (let [report (async/chan (async/sliding-buffer 100))
                    error (async/chan (async/sliding-buffer 100))
                    cast (fn [sigid msgs]
                           (doseq [{:keys [select chan]} (vals (:castees @rt))]
                             (when (and select (select sigid))
                               (doseq [m msgs] (async/>!! chan [sigid m])))))]
                (reset! rt {:status :paused :report report :error error :cast cast
                            :plans {} :in-chans {} :routes {} :mults {} :castees {} :running {}})
                (apply! @current)
                {:report-chan report :error-chan error})))))
      (stop [_]
        (locked
          (fn []
            (when-let [{:keys [report error running mults]} @rt]
              (send-control! rt (keys running) #::flow{:command ::flow/stop})
              ;; jolt: stop answers after every proc's loop has exited (its ::flow/stop
              ;; transition ran), bounded by :stop-timeout-ms; a proc still running is named
              (let [still (await-exits running stop-timeout-ms)]
                (doseq [[_ {:keys [control]}] running] (async/close! control))
                ;; jolt: a mult's go-loop parked on an open source holds a fiber: close them
                (doseq [[_ m] mults] (async/close! (:src m)))
                (async/close! error) (async/close! report)
                (reset! rt nil)
                (if (empty? still) true {::flow/running still}))))))
      (pause [_] (locked #(do (swap! rt assoc :status :paused)
                              (send-control! rt (keys (:running @rt)) #::flow{:command ::flow/pause}))))
      (resume [_] (locked #(do (swap! rt assoc :status :running)
                               (send-control! rt (keys (:running @rt)) #::flow{:command ::flow/resume}))))
      (ping [_ timeout-ms]
        (let [timeout (async/timeout timeout-ms)
              [n reply] (locked #(let [pids (keys (:running @rt))
                                       reply (async/chan (max 1 (count pids)))]
                                   (send-control! rt pids #::flow{:command ::flow/ping :reply-chan reply})
                                   [(count pids) reply]))
              ret-chan (fiber-go (async/take n reply))
              ret (loop [ret nil]
                    (let [[m _] (async/alts!! [ret-chan timeout])]
                      (if (some? m) (recur (assoc ret (::flow/pid m) m)) ret)))]
          ;; jolt: take's go-loop ends only when reply closes (a late pong is dropped)
          (async/close! reply)
          ret))
      (pause-proc [_ pid] (locked #(send-control! rt [pid] #::flow{:command ::flow/pause})))
      (resume-proc [_ pid] (locked #(send-control! rt [pid] #::flow{:command ::flow/resume})))
      (ping-proc [_ pid timeout-ms]
        (let [reply (async/chan 1)]
          (locked #(send-control! rt [pid] #::flow{:command ::flow/ping :reply-chan reply}))
          (let [[m _] (async/alts!! [reply (async/timeout timeout-ms)])]
            (async/close! reply)
            m)))
      (inject [_ [target id :as coord] msgs]
        (let [{:keys [cast]} (or @rt (throw (Exception. "flow not running")))
              do-io (if (= target ::flow/cast)
                      #(cast id msgs)
                      (let [c (or (get-in @rt [:in-chans coord]) (get-in @rt [:routes coord])
                                  (throw (ex-info "can't resolve channel with io-id" {:io-id coord})))]
                        #(doseq [m msgs] (async/>!! c m))))]
          ((futurize do-io {:exec :io}))))

      clojure.core.async.flow.impl.graph.Update
      (update-flow [_ d]
        (locked (fn []
                  (reset! current (select-keys d [:procs :conns]))
                  (if @rt (apply! d) {:running false})))))))

(defn- finish-outputs!
  "jolt (V1's one change to the proc loop): a stop that arrives while a step's outputs are
  being sent delivers the rest of them, each put bounded by stop-output-ms, instead of
  dropping them; one that misses its bound is reported."
  [outputs outs resolver cast stop-output-ms report]
  (doseq [[output msgs] outputs]
    (if (and (vector? output) (= (first output) ::flow/cast))
      (cast (second output) msgs)
      (when-let [outc (or (outs output) (spi/get-write-chan resolver output))]
        (doseq [m msgs]
          (let [[v _] (async/alts!! [[outc m] (async/timeout stop-output-ms)])]
            (when-not v (async/put! report {::flow/dropped-on-stop output}))))))))

(defn handle-command
  [pid pong status cmd]
  (let [transition #::flow{:stop :exit, :resume :running, :pause :paused}
        {::flow/keys [to command reply-chan]} cmd]
    (if (#{::flow/all pid} to)
      (do
        (when (= command ::flow/ping) (pong reply-chan))
        (or (transition command) status))
      status)))

(defn handle-transition
  "when transition, returns maybe new state"
  [transition status nstatus state]
  (if (not= status nstatus)
    (transition state (case nstatus
                            :exit ::flow/stop
                            :running ::flow/resume
                            :paused ::flow/pause))
    state))

(defn send-outputs [status state outputs outs resolver control handle-command transition cast]
  (loop [nstatus status, nstate state, outputs (seq outputs)]
    (if (or (nil? outputs) (= nstatus :exit))
      (do (when (and outputs (= nstatus :exit))
            (finish-outputs! outputs outs resolver cast 1000 (outs ::flow/report)))
          [nstatus nstate])
      (let [[output msgs] (first outputs)]
        (if (and (vector? output) (= (first output) ::flow/cast))
          (do (cast (second output) msgs)
              (recur nstatus nstate (next outputs)))
          (if-let [outc (or (outs output) (spi/get-write-chan resolver output))]
            (let [[nstatus nstate]
                  (loop [nstatus nstatus, nstate nstate, msgs (seq msgs)]
                    (if (or (nil? msgs) (= nstatus :exit))
                      (do (when (and msgs (= nstatus :exit))
                            (finish-outputs! [[output msgs]] outs resolver cast 1000 (outs ::flow/report)))
                          [nstatus nstate])
                      (let [[v c] (async/alts!!
                                   [control [outc (first msgs)]]
                                   :priority true)]
                        (if (= c control)
                          (let [nnstatus (handle-command nstatus v)
                                nnstate (handle-transition transition nstatus nnstatus nstate)]
                            (recur nnstatus nnstate msgs))
                          (recur nstatus nstate (next msgs))))))]
              (recur nstatus nstate (next outputs)))
            (recur nstatus nstate  (next outputs))))))))

(defn proc
  "see lib ns for docs"
  [step {:keys [workload compute-timeout-ms] :or {compute-timeout-ms 5000}}]
  (let [{:keys [params ins ping-map-fn] :or {ping-map-fn identity} :as desc} (step)
        workload (or workload (:workload desc) :mixed)]
    ;;(assert (or (not params) init) "must have :init if :params")
    (reify
      clojure.core.protocols/Datafiable
      (datafy [_]
        (let [{:keys [params ins outs]} desc]
          (walk/postwalk datafy {:step step :desc desc})))
      spi/ProcLauncher
      (describe [_] desc)
      (start [_ {:keys [pid args ins outs resolver cast]}]
        (assert (or (not params) args) "must provide :args if :params")
        (let [transform (if (= workload :compute)
                          #(.get ^Future ((futurize step {:exec (spi/get-exec resolver :compute)}) %1 %2 %3)
                                 compute-timeout-ms TimeUnit/MILLISECONDS)
                          step)
              exs (spi/get-exec resolver (if (= workload :mixed) :mixed :io))
              state (step args)
              ins (into (or ins {}) (::flow/in-ports state))
              outs (into (or outs {}) (::flow/out-ports state))
              io-id (zipmap (concat (vals ins) (vals outs)) (concat (keys ins) (keys outs)))
              control (::flow/control ins)
              casts (::flow/casts ins)
              read-ins (dissoc ins ::flow/control ::flow/casts)
              run
              #(loop [status :paused, state state, count 0, read-ins read-ins]
                 (let [pong (fn [c]
                              (let [pins (dissoc ins ::flow/control ::flow/casts)
                                    pouts (dissoc outs ::flow/error ::flow/report)]
                                (async/>!! c (assoc (walk/postwalk datafy
                                                     #::flow{:pid pid, :status status
                                                             :count count
                                                             :ins pins :outs pouts})
                                                    ::flow/state (ping-map-fn state)))))
                       handle-command (partial handle-command pid pong)
                       [nstatus nstate count read-ins]
                       (try
                         (if (= status :paused)
                           (let [nstatus (handle-command status (async/<!! control))
                                 nstate (handle-transition step status nstatus state)]
                             [nstatus nstate count read-ins])
                           ;;:running
                           (let [ ;;TODO rotate/randomize after control per normal alts?
                                 read-chans (let [ipred (or (::flow/input-filter state) identity)]
                                              (reduce-kv (fn [ret cid chan]
                                                           (if (ipred cid)
                                                             (conj ret chan)
                                                             ret))
                                                         [control casts] read-ins))
                                 [msg c] (async/alts!! read-chans :priority true)
                                 cid (io-id c)]
                             (if (= c control)
                               (let [nstatus (handle-command status msg)
                                     nstate (handle-transition step status nstatus state)]
                                 [nstatus nstate count read-ins])
                               (try
                                 (let [[nstate outputs]
                                       (if (= c casts) ;;[sigid msg]
                                         (transform state (first msg) (second msg))
                                         (transform state cid msg))
                                       [nstatus nstate]
                                       (send-outputs status nstate outputs outs
                                                     resolver control handle-command step cast)]
                                   [nstatus nstate (inc count) (if (some? msg)
                                                                 read-ins
                                                                 (dissoc read-ins cid))])
                                 (catch Throwable ex
                                   (async/>!! (outs ::flow/error)
                                              #::flow{:pid pid, :status status, :state state,
                                                      :count (inc count), :cid cid, :msg msg :op :step, :ex ex})
                                   [status state count read-ins])))))
                         (catch Throwable ex
                           (async/>!! (outs ::flow/error)
                                      #::flow{:pid pid, :status status, :state state, :count (inc count), :ex ex})
                           [status state count read-ins]))]
                   (when-not (= nstatus :exit) ;;fall out
                     (recur nstatus nstate (long count) read-ins))))]
          ((futurize run {:exec exs})))))))
