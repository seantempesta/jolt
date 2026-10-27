;; jolt.async.remote gate: a core.async channel whose other end is another OS
;; process, and Flow wired across one.
;;
;; Run: bin/jolt run test/chez/remote-chan-test.clj   (prints REMOTE-CHAN-TEST OK)
;;
;; Children come from a fork server: a single-threaded process forked from this one
;; before anything here starts a thread, as a supervisor's workers usually do. Each
;; child's descriptors reach the server by SCM_RIGHTS (send-fd!), so passing an end to
;; another process is proven on every spawn. Why a server: Chez's parallel collector
;; keeps sweeper threads, made the first time a collection finds other threads waiting;
;; a fork does not copy them, and a child of a process that has them hangs in its
;; first collection that has a second active thread (seen here: S_gc_par_entry ->
;; run_sweeper waiting forever). A child ends with _exit, never returning into this file.
(ns remote-chan-test
  (:require [clojure.core.async :as a]
            [clojure.core.async.flow :as flow]
            [jolt.async.remote :as r]
            [jolt.async.remote.flow :as rflow]
            [jolt.ffi :as ffi]
            [jolt.socket.native :as native]
            [jolt.scheme]))

(def failures (atom []))
(defmacro check [label ok? & [detail]]
  `(do (print (str "  .. " ~label "\n")) (flush)
       (let [ok# (try ~ok? (catch Throwable t# (swap! failures conj (str ~label ": threw " (ex-message t#))) ::threw))]
         (when-not (or (true? ok#) (= ::threw ok#))
           (swap! failures conj (str ~label ": " (pr-str ~detail)))))))

(defn await-until [cap-ms pred]
  (let [deadline (+ (System/nanoTime) (* cap-ms 1000000))]
    (loop [] (or (pred) (if (>= (System/nanoTime) deadline) (pred) (do (Thread/sleep 2) (recur)))))))

(defn take-within [ms ch] (first (a/alts!! [ch (a/timeout ms)])))
(defn ms-since [t0] (/ (- (System/nanoTime) t0) 1e6))

;; ---- processes -------------------------------------------------------------------
(def ^:private qfork
  (jolt.scheme/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)])
       (lambda ()
         (let loop ()
           (disable-interrupts)
           (mutex-acquire #%$tc-mutex)
           (if (= (jolt-active-threads) 1)
               (let ([pid (fork)]) (mutex-release #%$tc-mutex) (enable-interrupts) pid)
               (begin (mutex-release #%$tc-mutex) (enable-interrupts)
                      (sleep (make-time 'time-duration 50000 0)) (loop))))))"))
(ffi/defcfn c-exit "_exit" [:int] :void)
(ffi/defcfn c-kill "kill" [:int :int] :int)
(ffi/defcfn c-waitpid "waitpid" [:int :pointer :int] :int)
(ffi/defcfn c-fcntl "fcntl" [:int :int :varargs :int] :int)

(defn- write-bytes! [fd ^bytes b]
  (let [p (ffi/alloc (max 1 (alength b)))]
    (try (ffi/write-array p b 0 (alength b)) (first (native/c-send fd p (alength b) native/msg-nosignal))
         (finally (ffi/free p)))))
(defn- read-bytes! [fd n]
  (let [p (ffi/alloc n)]
    (try (loop [got 0]
           (if (= got n) (ffi/read-array p n)
               (let [[r _] (native/c-recv fd (+ p got) (- n got) 0)] (if (pos? r) (recur (+ got r)) nil))))
         (finally (ffi/free p)))))

(defn frame-bytes
  "One frame as jolt.async.remote writes it with the EDN codec, built by hand: for a
  process that must not start a thread."
  [v]
  (let [^bytes b (.getBytes ^String (pr-str v) "UTF-8") n (alength b) f (byte-array (+ 4 n))]
    (dotimes [i 4] (aset f i (unchecked-byte (bit-shift-right n (* 8 i)))))
    (System/arraycopy b 0 f 4 n)
    f))

;; ---- the child bodies (defined before the server is forked, so it has them) ----------
(defn echo-body [f]
  (fn [[fd]]
    (let [e (r/open fd)]
      (loop [] (when-some [v (a/<!! (:in e))] (a/>!! (:out e) (f v)) (recur)))
      (a/close! (:out e))
      (Thread/sleep 50))))

(def bodies
  {:echo-inc (echo-body inc)
   :echo-x10 (echo-body #(* 10 %))
   :hello-sleep (fn [[fd]] (let [c (r/open fd)] (a/>!! (:out c) :hello) (Thread/sleep 60000)))
   :three-then-close (fn [[fd]] (let [c (r/open fd)] (dotimes [i 3] (a/>!! (:out c) i)) (a/close! (:out c)) (Thread/sleep 200)))
   ;; A: forks a test child T, which must not hold A's socket. T runs no thread (its
   ;; parent A does, so its collector cannot), so it writes its report frame by hand.
   :fork-a (fn [[fd rep-fd]]
             (let [c (r/open fd)]
               (when (zero? (qfork))
                 (write-bytes! rep-fd (frame-bytes {:fd-flags (c-fcntl (:fd c) 1 0)}))
                 (Thread/sleep 3000)
                 (c-exit 0))
               (dotimes [i 200] (a/>!! (:out c) i))
               (Thread/sleep 60000)))
   :flow-agent (fn [[fd sfd]]
                 (let [c (r/open fd) sc (r/open sfd)]
                   (a/>!! (:out sc) [[:note :in] :hello])
                   (a/>!! (:out sc) [[:gone :in] :lost])
                   (a/>!! (:out sc) [[:note :in] :after])
                   (loop [] (when-some [v (a/<!! (:in c))] (a/>!! (:out c) (* 2 v)) (recur)))
                   (a/close! (:out c)) (Thread/sleep 100)))})

;; ---- the fork server ---------------------------------------------------------------
;; Requests on ctl: [n-fds body-index], then n descriptors by SCM_RIGHTS; it forks a
;; child running that body with them, and answers the pid (4 bytes). It reaps.
(def body-keys (vec (sort (keys bodies))))
(def ctl
  (let [[mine theirs] (r/socketpair)
        pid (qfork)]
    (if (zero? pid)
      (do (r/close-fd! mine)
          (loop []
            (let [st (ffi/alloc 4)] (while (pos? (c-waitpid -1 st 1))) (ffi/free st))
            (if-let [hdr (read-bytes! theirs 2)]
              (let [fds (vec (repeatedly (aget hdr 0) #(r/recv-fd! theirs)))
                    body (bodies (body-keys (aget hdr 1)))
                    child (qfork)]
                (if (zero? child)
                  (do (r/close-fd! theirs)
                      (try (body fds) (c-exit 0) (catch Throwable _ (c-exit 3))) (c-exit 3))
                  (do (run! r/close-fd! fds)
                      (write-bytes! theirs (let [b (byte-array 4)] (dotimes [i 4] (aset b i (unchecked-byte (bit-shift-right child (* 8 i))))) b))
                      (recur))))
              (c-exit 0))))
      (do (r/close-fd! theirs) (r/clofork! mine)))))

(defn spawn!
  "Run body k in a fresh child of the fork server with descriptors fds (closed here
  once the server has answered). Answers the child's pid."
  [k & fds]
  (write-bytes! ctl (byte-array [(count fds) (.indexOf ^java.util.List body-keys k)]))
  (doseq [fd fds] (r/send-fd! ctl fd))
  (let [b (read-bytes! ctl 4)]
    ;; close our copies only once the server has them: see send-fd!
    (run! r/close-fd! fds)
    (reduce + (map-indexed (fn [i x] (bit-shift-left (bit-and x 0xff) (* 8 i))) b))))

;; ---- 1. FIFO per direction, in one process and across a fork ----------------------
(doseq [pump [:thread :fiber]]
  (let [[x fd] (r/remote-pair {:pump pump}) y (r/open fd {:pump pump})
        n 5000 vs (vec (for [i (range n)] {:i i :s (str "v" i) :v [i (* 2 i)]}))]
    (a/thread (doseq [v vs] (a/>!! (:out x) v)))
    (check (str "FIFO, one process, " pump ": 5,000 maps arrive whole and in order")
           (= vs (vec (repeatedly n #(take-within 5000 (:in y))))))
    (r/close! x) (r/close! y)))

(let [[e fd] (r/remote-pair) pid (spawn! :echo-inc fd)
      n 10000]
  (a/thread (dotimes [i n] (a/>!! (:out e) i)) (a/close! (:out e)))
  (let [got (loop [acc (transient [])] (if-some [v (take-within 5000 (:in e))] (recur (conj! acc v)) (persistent! acc)))]
    (check "FIFO across a fork (the descriptor passed by SCM_RIGHTS): 10,000 values out and back, in order, then the child's end frame"
           (= (range 1 (inc n)) got) {:count (count got) :head (take 5 got)})))

;; close! after both directions ended must not touch the descriptor number: by then it
;; may be a new socket's (the bug: close! shut down whatever socket had reused it)
(let [[x fd] (r/remote-pair) y (r/open fd)]
  (a/close! (:out y)) (a/close! (:out x))
  (check "both directions ended: both :in channels close" (and (nil? (take-within 5000 (:in x))) (nil? (take-within 5000 (:in y)))))
  (await-until 2000 #(neg? (c-fcntl (:fd x) 1 0)))        ; the pumps closed x's descriptor
  (let [[p pfd] (r/remote-pair) q (r/open pfd)]
    (r/close! x) (r/close! y)                               ; late closes of the old ends
    (a/>!! (:out p) :still-open)
    (check "a late close! leaves a new socket that reused the number alone"
           (= :still-open (take-within 5000 (:in q))) {:old (:fd x) :new [(:fd p) (:fd q)]})
    (r/close! p) (r/close! q)))

;; ---- 2. back-pressure: nothing lost, and the sender really waits -------------------
(let [[e fd] (r/remote-pair {:buf 4 :sock-buf 8192}) y (r/open fd {:buf 4})
      n 20000 sent (atom 0)
      payload (apply str (repeat 100 "x"))
      sender (a/thread (dotimes [i n] (a/>!! (:out e) [i payload]) (swap! sent inc)))]
  (Thread/sleep 300)                       ; nobody takes from y meanwhile
  (let [stalled @sent]
    (check "back-pressure: with nobody taking, the sender stops (in flight is bounded)"
           (< stalled 1000) {:sent-while-stopped stalled})
    (let [got (vec (repeatedly n #(take-within 5000 (:in y))))]
      (check "back-pressure: after the reader resumes, all 20,000 arrive in order"
             (= (range n) (map first got)) {:count (count (filter some? got))}))
    (println "   sent while the reader was stopped:" stalled "of" n))
  (r/close! e) (r/close! y))

;; ---- 3. peer death is close ---------------------------------------------------------
(let [[e fd] (r/remote-pair)
      pid (spawn! :hello-sleep fd)]
  (check "the child's first value arrives" (= :hello (take-within 5000 (:in e))))
  (let [t0 (System/nanoTime)]
    (c-kill pid 9)
    (let [v (take-within 5000 (:in e)) ms (ms-since t0)]
      (check "SIGKILL of the peer: a pending take answers nil" (and (nil? v) (< ms 2000)) {:v v :ms ms})
      (println "   death seen as close after" ms "ms")))
  (check "after the peer's death a put answers false" (false? (a/>!! (:out e) 1))))

;; close! of several ends, then at once a new child whose descriptor is in flight to
;; the fork server (SCM_RIGHTS): the new child must still connect. close! used to
;; shutdown(SHUT_RD) its socket, and on macOS that broke the socket in flight (its
;; child read EOF at once), about one batch in two.
(let [ok (for [_ (range 6)]
           (let [ends (vec (for [_ (range 3)] (let [[e fd] (r/remote-pair)] (spawn! :echo-inc fd) e)))]
             (doseq [e ends] (a/>!! (:out e) 1))
             (let [firsts (mapv #(take-within 5000 (:in %)) ends)]
               (doseq [e ends] (r/close! e))
               firsts)))]
  (check "six batches: close! three ends, spawn the next three at once; every child answers"
         (every? #(= [2 2 2] %) ok) (vec ok)))

;; a value put before the peer's end frame still arrives, then nil (graceful close)
(let [[e fd] (r/remote-pair)]
  (spawn! :three-then-close fd)
  (check "graceful close: the values, then nil"
         (= [0 1 2 nil] (vec (repeatedly 4 #(take-within 5000 (:in e)))))))

;; ---- 4. alts! over local and remote channels ---------------------------------------
(let [[e fd] (r/remote-pair) pid (spawn! :echo-x10 fd)
      local (a/chan 1)]
  (a/>!! (:out e) 1)
  (let [[v c] (a/alts!! [local (:in e)])]
    (check "alts!! takes the remote value when only the remote has one" (and (= 10 v) (= c (:in e))) [v]))
  (a/>!! local :l)
  (let [[v c] (a/alts!! [local (:in e)] :priority true)]
    (check "alts!! takes the local value when only the local has one" (and (= :l v) (= c local)) [v]))
  (let [[v c] (a/alts!! [[(:out e) 2] (a/timeout 2000)])]
    (check "alts!! puts to a remote :out" (and (true? v) (= c (:out e)))))
  (check "the alts! put arrives" (= 20 (take-within 5000 (:in e))))
  ;; on fibers, in a go block, both sides of the choice racing
  (let [res (a/<!! (a/go (let [_ (a/>! (:out e) 3)
                               [v c] (a/alts! [local (:in e) (a/timeout 5000)])]
                           [v (= c (:in e))])))]
    (check "alts! in a go block over local + remote" (= [30 true] res) res))
  (let [n 1000
        lo (a/chan 16)
        _ (a/thread (dotimes [i n] (a/>!! lo [:l i])))
        _ (a/thread (dotimes [i n] (a/>!! (:out e) i)))
        got (loop [acc [] k 0]
              (if (= k (* 2 n)) acc
                  (let [[v _] (a/alts!! [lo (:in e) (a/timeout 5000)])] (if (nil? v) acc (recur (conj acc v) (inc k))))))]
    (check "alts!! over a busy local and a busy remote: every value once, each stream in order"
           (and (= (* 2 n) (count got))
                (= (range n) (map second (filter vector? got)))
                (= (map #(* 10 %) (range n)) (remove vector? got)))
           {:count (count got)}))
  (a/close! (:out e)))

;; ---- 5. fork: a test child neither steals frames nor hides its parent's death -------
;; The test owns end e; its peer is A. A forks T, a "test child" that lives 3 s and
;; tries to read A's socket. A then sends 200 values and waits. The test receives all
;; 200 (T stole none), kills A, and sees close at once although T is still alive.
(let [[e fd] (r/remote-pair)
      [rep rep-fd] (r/remote-pair)            ; T reports what it found here
      a-pid (spawn! :fork-a fd rep-fd)]
  (check "the forked test child does not hold its parent's socket (fcntl answers -1)"
         (= {:fd-flags -1} (take-within 5000 (:in rep))))
  (let [got (vec (repeatedly 200 #(take-within 5000 (:in e))))]
    (check "all 200 frames reach the parent's peer; none stolen by the test child" (= (range 200) got)))
  (let [t0 (System/nanoTime)]
    (c-kill a-pid 9)
    (let [v (take-within 5000 (:in e)) ms (ms-since t0)]
      (check "the parent's death reads as close while its test child is still alive"
             (and (nil? v) (< ms 1000)) {:ms ms})))
  (r/close! rep))

;; ---- 6. Flow wired across a forked child --------------------------------------------
;; :agent is a process in a child, in the graph as a channel proc: [:agent :in] is its
;; end's :out, [:agent :out] its :in. The child doubles what it gets, and on a second
;; end it sends addressed messages [coord msg] straight to procs of the graph.
(defn src ([] {:outs {:out ""} :workload :io}) ([a] (assoc a ::flow/in-ports {:in (:ch a)})) ([s _] s) ([s _ m] [s {:out [m]}]))
(defn sink ([] {:ins {:in ""} :workload :io}) ([a] a) ([s _] s) ([s _ m] (swap! (:seen s) conj m) [s nil]))
(defn note ([] {:ins {:in ""} :workload :io}) ([a] a) ([s _] s) ([s _ m] (swap! (:seen s) conj m) [s nil]))

(let [[e fd] (r/remote-pair) [s sfd] (r/remote-pair)
      pid (spawn! :flow-agent fd sfd)
      ch (a/chan 64) seen (atom []) notes (atom [])
      g (flow/create-flow
          {:procs {:src {:proc (flow/process #'src) :args {:ch ch}}
                   :agent (rflow/proc {:ins {:in (:out e)} :outs {:out (:in e)} :sends {:sends (:in s)}})
                   :sink {:proc (flow/process #'sink) :args {:seen seen}}
                   :note {:proc (flow/process #'note) :args {:seen notes}}}
           :conns [[[:src :out] [:agent :in]] [[:agent :out] [:sink :in]]]})
      {:keys [report-chan error-chan]} (flow/start g)
      _ (flow/resume g)
      n 2000]
  (a/thread (dotimes [i n] (a/>!! ch i)))
  (check "Flow across a fork: src -> [:agent :in] -> child -> [:agent :out] -> sink, all in order"
         (and (await-until 10000 #(= n (count @seen))) (= (map #(* 2 %) (range n)) @seen))
         {:count (count @seen)})
  (check "addressed sends from the child reach [:note :in], in order"
         (and (await-until 5000 #(= 2 (count @notes))) (= [:hello :after] @notes)) @notes)
  (let [reports (loop [acc []] (if-some [m (take-within 500 report-chan)] (recur (conj acc m)) acc))]
    (check "the send to a pid not in the flow is reported as undeliverable, not thrown"
           (some #(= [:gone :in] (::flow/undeliverable %)) reports) reports))
  (check "no errors on the error channel" (nil? (take-within 100 error-chan)))
  ;; the child dies: its out-port closes and the report says so
  (c-kill pid 9)
  (check "the child's death is reported as its out-port closing"
         (await-until 5000 #(some (fn [m] (= :out (::flow/closed m))) [(take-within 50 report-chan)])))
  (check "a send to the dead child's in-port does not wedge the graph"
         (do (a/>!! ch 1) (Thread/sleep 50) (some? (flow/ping-proc g :src))))
  (check "stop answers true" (true? (flow/stop g))))

;; ---- 7. Flow: an output to a proc that left the graph is dropped and reported -------
(defn hop ([] {:ins {:in ""} :workload :io}) ([a] a) ([s _] s)
  ([s _ m] [s {[(:to m) :in] [(:v m)]}]))
(let [seen (atom [])
      cfg {:procs {:hop {:proc (flow/process #'hop)}
                   :b {:proc (flow/process #'sink) :args {:seen seen}}}
           :conns []}
      g (flow/create-flow cfg)
      {:keys [report-chan error-chan]} (flow/start g)
      _ (flow/resume g)]
  @(flow/inject g [:hop :in] [{:to :b :v 1}])
  (check "an addressed output reaches a live proc" (await-until 5000 #(= [1] @seen)))
  (flow/update! g (update cfg :procs dissoc :b))
  @(flow/inject g [:hop :in] [{:to :b :v 2}])
  (check "an output to a proc that left: reported as ::flow/undeliverable"
         (= #::flow{:undeliverable [:b :in] :pid :hop} (take-within 5000 report-chan)))
  (check "...and the sender's step did not throw" (nil? (take-within 100 error-chan)))
  (flow/update! g cfg)
  @(flow/inject g [:hop :in] [{:to :b :v 3}])
  (check "the sender is still running: once :b is back, the next send arrives"
         (await-until 5000 #(= 3 (last @seen))) @seen)
  (flow/stop g))

(if (empty? @failures)
  (println "REMOTE-CHAN-TEST OK")
  (do (println "REMOTE-CHAN-TEST FAILED" (count @failures))
      (doseq [f @failures] (println "  FAIL" f))
      (System/exit 1)))
(System/exit 0)
