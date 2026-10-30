;; jolt.async.remote — a core.async channel whose other end is in another OS process.
;;
;; An end is two ORDINARY channels and one AF_UNIX stream socket:
;;
;;   {:in  ch   ; values the peer put, in its order: take!, <!, <!!, poll!, alts!
;;    :out ch   ; values for the peer: put!, >!, >!!, offer!, alts! puts, close!
;;    :fd  n}
;;
;; Two pumps move values between the channels and the socket: a writer takes from
;; :out, encodes and writes frames; a reader reads frames, decodes and puts on :in.
;; Because both halves are the runtime's own channels, every channel op, alts! over
;; any mix of local and remote channels, mult/pipe/Flow, and fiber parking work
;; unchanged; nothing in the channel core knows a socket exists.
;;
;; Semantics:
;;   - FIFO per direction (one stream socket).
;;   - Back-pressure end to end: a full :in stops the reader, the socket's receive
;;     and send buffers fill, the writer blocks, :out fills, and a put parks. Nothing
;;     is dropped. In flight at most: :out's buffer + the kernel buffers + :in's buffer.
;;   - (close! (:out end)) sends an end frame after the values already put; the peer
;;     takes them, then nil. The peer's :out stays usable (a half close).
;;   - Peer death (EOF before an end frame, or EPIPE/ECONNRESET on write) closes both
;;     halves: a pending or later take answers nil once :in is drained, a put answers false.
;;   - alts!: both halves are local channels, so alts! is the runtime's own. The limit
;;     is stated plainly: a put to :out commits when the value enters :out's buffer,
;;     not when the peer takes it. There is no two-phase protocol across processes.
;;   - Fork: this process's end of the socket is not held by a child forked later (a test
;;     child, a clone): it cannot read this end's frames, write into its stream, or keep the
;;     peer from seeing this process die. macOS marks the descriptor FD_CLOFORK; Linux has no
;;     such flag, so the child's jolt.host/after-fork-child! closes every end it inherited
;;     (clofork! registers the descriptor for it).
;;   - A descriptor travels with a value: put a value whose metadata holds ::fd (an
;;     open descriptor of this process) on :out, and it arrives on the peer's :in with
;;     ::fd in its metadata naming the peer's own descriptor for the same file
;;     (SCM_RIGHTS, sent with that value's frame's first bytes, so it keeps its place
;;     among the values around it). The sender may close its own once the value is put;
;;     the receiver owns the one it got. A value that carries one must take metadata (a
;;     map, a vector): its frame is written alone, flagged in its length word.
;;   - A raw descriptor (the second value of remote-pair) is inherited by a child that
;;     is forked next, or handed to any process with send-fd!; that process calls open.
;;
;; Pumps (the :pump option):
;;   :thread (default) — one OS thread each, blocking in recv(2) / on the channel: the
;;     kernel wakes the reader itself, the shortest path for one busy end.
;;   :fiber — fibers over a non-blocking socket, parked on jolt.io-poller's one
;;     readiness thread: thousands of ends cost no thread each.
;;
;; Forking and threads. fork(2) copies only the calling thread. A child that opens
;; an end runs pump threads, so it must come from a process that is safe to fork
;; and then run threads in: one with a single active Chez thread at the fork and no
;; parallel-collector sweeper threads yet (Chez makes those the first time a
;; collection finds other threads waiting, and keeps them; a child of a process that
;; has them hangs in its first such collection). A single-threaded fork server, forked
;; before anything starts a thread, is that process. A child forked from a threaded
;; process (a test child of a worker) can still use the descriptors it inherited raw,
;; with no pump.
;;
;; The codec is a seam: {:encode (fn [v] byte-array) :decode (fn [byte-array] v)}.
;; The default is EDN (edn-codec); a faster binary codec plugs in with :codec. A
;; value the codec cannot encode or decode is a defect: the pump that met it closes
;; both halves and rethrows on its own thread.
(ns jolt.async.remote
  (:require [clojure.core.async :as a]
            [clojure.edn :as edn]
            [jolt.ffi :as ffi]
            [jolt.io-poller :as poller]
            [jolt.socket.native :as native]
            [clojure.string :as str]
            [jolt.fibers]))

(ffi/load-library)

(def ^:private macos? (str/includes? (str/lower-case (or (System/getProperty "os.name") "")) "mac"))

(ffi/defcfn ^:private c-socketpair "socketpair" [:int :int :int :pointer] :int {:capture-native-error true})
(ffi/defcfn ^:private c-shutdown "shutdown" [:int :int] :int)
(ffi/defcfn ^:private c-sendmsg "sendmsg" [:int :pointer :int] :ssize_t {:blocking true :capture-native-error true})
(ffi/defcfn ^:private c-recvmsg "recvmsg" [:int :pointer :int] :ssize_t {:blocking true :capture-native-error true})

(def ^:private AF-UNIX 1)
(def ^:private SOCK-STREAM 1)
(def ^:private SO-SNDBUF (if macos? 0x1001 7))
(def ^:private SO-RCVBUF (if macos? 0x1002 8))
(def ^:private F-GETFD 1)
(def ^:private F-SETFD 2)
(def ^:private F-GETFL 3)
(def ^:private F-SETFL 4)
(def ^:private O-NONBLOCK (if macos? 0x4 0x800))
;; macOS's close-on-fork. Linux has no such flag: there the child closes the descriptors it
;; inherited itself, in jolt.host/after-fork-child! (close-held! below).
(def ^:private FD-CLOFORK (when macos? 2))
(def ^:private SCM-RIGHTS 1)
(def ^:private END 0xFFFFFFFF)          ; the length word of an end frame
(def ^:private CARRIES 0x80000000)      ; a length word's flag: this frame carries one descriptor
(def ^:private LENGTH 0x7FFFFFFF)       ; a length word's length

;; ---- the codec seam -----------------------------------------------------------

(def edn-codec
  "Values as UTF-8 EDN: readable by anything, and the slowest choice."
  {:encode (fn [v] (.getBytes ^String (pr-str v) "UTF-8"))
   :decode (fn [^bytes b] (edn/read-string (String. b "UTF-8")))})

;; ---- descriptors --------------------------------------------------------------

(defn- fcntl [fd cmd arg] (first (native/c-fcntl fd cmd arg)))

(def ^:private held
  "Where there is no FD_CLOFORK: the descriptors marked close-on-fork, which a forked child
  closes (close-held!). Until close-fd! or the end's finish! closes one."
  (atom #{}))

(defn clofork!
  "Mark fd close-on-fork: no child forked from now on holds it (macOS: the flag; Linux: the
  child's after-fork-child! closes it, until this process closes fd with close-fd!).
  Answers fd."
  [fd]
  (if FD-CLOFORK
    (fcntl fd F-SETFD (bit-or FD-CLOFORK (fcntl fd F-GETFD 0)))
    (swap! held conj fd))
  fd)

(defn clofork?
  "Is fd close-on-fork in this process? (In a child forked on Linux the inherited ends are
  closed, so it answers false for them there.)"
  [fd]
  (if FD-CLOFORK (pos? (bit-and FD-CLOFORK (fcntl fd F-GETFD 0))) (contains? @held fd)))

(defn- set-int-opt! [fd opt v]
  (let [b (ffi/alloc 4)]
    (try (ffi/write b :int v) (first (native/c-setsockopt fd native/sol-socket opt b 4))
         (finally (ffi/free b)))))

(defn socketpair
  "[a b]: a connected AF_UNIX stream pair, raw descriptors. A write to a dead peer
  answers EPIPE, never a signal. opts: :sock-buf, bytes of each socket's send and
  receive buffer (nil keeps the system's default)."
  ([] (socketpair {}))
  ([{:keys [sock-buf]}]
   (let [sv (ffi/alloc 8)]
     (try
       (let [[r e] (c-socketpair AF-UNIX SOCK-STREAM 0 sv)]
         (when-not (zero? r) (throw (ex-info "socketpair failed" {:errno e})))
         (let [fds [(ffi/read sv :int 0) (ffi/read sv :int 4)]]
           (doseq [fd fds]
             (when native/so-nosigpipe (set-int-opt! fd native/so-nosigpipe 1))
             (when sock-buf (set-int-opt! fd SO-SNDBUF sock-buf) (set-int-opt! fd SO-RCVBUF sock-buf)))
           fds))
       (finally (ffi/free sv))))))

(defn close-fd! "Close a raw descriptor (one handed on to another process)." [fd]
  (swap! held disj fd)
  (first (native/c-close fd)))

(defn- close-held!
  "In a forked child: close every end this process held (Linux, which has no FD_CLOFORK), so the
  child holds none of its parent's sockets. The end's pumps are the parent's threads: gone."
  []
  (let [fds @held]
    (reset! held #{})
    (doseq [fd fds] (native/c-close fd))))
(when-not FD-CLOFORK (jolt.host/after-fork-add! close-held!))

;; struct msghdr (LP64): name @0, namelen @8, iov @16, iovlen @24, control @32,
;; controllen @40, flags @44 (48 bytes; on Linux iovlen and controllen are size_t, flags is  and the
;; struct is 56 bytes, so the iovec that follows it starts at 56 on both); iovec: base @0, len @8; one cmsghdr with one
;; int: len @0, level @4, type @8, fd @12 (CMSG_SPACE(4) = 16 on both platforms'
;; LP64 ABIs: darwin aligns to 4, glibc to 8 with a 16-byte header — see cmsg-*).
(def ^:private cmsg-hdr (if macos? 12 16))
(def ^:private cmsg-space (if macos? 16 24))

(defn send-fd!
  "Hand descriptor fd to the process at the other end of unix socket s (SCM_RIGHTS),
  with one byte of data. The receiver gets its own descriptor for the same socket.
  Close this one only after the receiver has answered that it holds it: on macOS a
  socket whose only reference is in flight was measured to be discarded (its
  receiver reads EOF at once) when other unix sockets of the process close
  meanwhile. Answers true, or the errno."
  [s fd]
  (let [m (ffi/alloc (+ 48 16 8 cmsg-space))
        iov (+ (ffi/address m) 48) one (+ iov 16) ctl (+ one 8)]
    (try
      (dotimes [i (+ 48 16 8 cmsg-space)] (ffi/write m :uint8 0 i))
      (ffi/write one :uint8 33)
      (ffi/write iov :uint64 one 0) (ffi/write iov :uint64 1 8)
      (if macos? (ffi/write ctl :uint32 (+ cmsg-hdr 4) 0) (ffi/write ctl :uint64 (+ cmsg-hdr 4) 0))
      (ffi/write ctl :int native/sol-socket (- cmsg-hdr 8)) (ffi/write ctl :int SCM-RIGHTS (- cmsg-hdr 4))
      (ffi/write ctl :int fd cmsg-hdr)
      (ffi/write m :uint64 iov 16) (ffi/write m :int 1 24)
      (ffi/write m :uint64 ctl 32) (if macos? (ffi/write m :uint32 cmsg-space 40) (ffi/write m :uint64 cmsg-space 40))
      (let [[r e] (c-sendmsg s m 0)] (if (pos? r) true e))
      (finally (ffi/free m)))))

(defn recv-fd!
  "Receive one descriptor sent with send-fd! on unix socket s (blocking): the new fd,
  or nil when s reached EOF or carried no descriptor."
  [s]
  (let [m (ffi/alloc (+ 48 16 8 cmsg-space))
        iov (+ (ffi/address m) 48) one (+ iov 16) ctl (+ one 8)]
    (try
      (dotimes [i (+ 48 16 8 cmsg-space)] (ffi/write m :uint8 0 i))
      (ffi/write iov :uint64 one 0) (ffi/write iov :uint64 1 8)
      (ffi/write m :uint64 iov 16) (ffi/write m :int 1 24)
      (ffi/write m :uint64 ctl 32) (if macos? (ffi/write m :uint32 cmsg-space 40) (ffi/write m :uint64 cmsg-space 40))
      (let [[r _] (loop [] (let [[r e] (c-recvmsg s m 0)] (if (and (neg? r) (native/eintr? e)) (recur) [r e])))]
        (when (and (pos? r) (= SCM-RIGHTS (ffi/read ctl :int (- cmsg-hdr 4))))
          (ffi/read ctl :int cmsg-hdr)))
      (finally (ffi/free m)))))

;; ---- the socket, as a buffered frame reader and a frame writer ----------------
;; A frame is [u32 length, little-endian][body]; length END is an end frame.
;; rd: long-array [buf cap lo hi] over foreign memory, grown to the largest frame.

;; An end's buffers start small and grow to the largest frame (rd-room!, the writer's
;; room!): a process holding a thousand mostly idle ends (a root of agents) pays 4 KB each,
;; not 64 KB.
(def ^:private start-bytes 4096)

(defn- rd-new [] (long-array [(ffi/address (ffi/alloc start-bytes)) start-bytes 0 0]))

(defn- rd-room!
  "Compact the buffer, then grow it until `need` bytes fit from its start."
  [^longs st need]
  (let [buf (aget st 0) cap (aget st 1) lo (aget st 2) live (- (aget st 3) lo)]
    (when (pos? lo)
      (when (pos? live) (ffi/copy (+ buf lo) buf live))
      (aset st 2 0) (aset st 3 live))
    (when (> need cap)
      (let [ncap (loop [c cap] (if (< c need) (recur (* 2 c)) c))
            nb (ffi/address (ffi/alloc ncap))]
        (when (pos? live) (ffi/copy buf nb live))
        (ffi/free buf)
        (aset st 0 nb) (aset st 1 ncap)))))

(defn- rd-frame
  "The next whole frame in the buffer: a byte-array, [::fd byte-array] for one that carries a
  descriptor, ::end, or nil (not all there)."
  [^longs st]
  (let [lo (aget st 2) avail (- (aget st 3) lo)]
    (when (>= avail 4)
      (let [buf (aget st 0) w (ffi/read (+ buf lo) :uint32)]
        (if (= w END)
          (do (aset st 2 (+ lo 4)) ::end)
          (let [n (bit-and w LENGTH)]
            (when (>= avail (+ 4 n))
              (let [b (ffi/read-array (+ buf lo 4) n)]
                (aset st 2 (+ lo 4 n))
                (if (zero? (bit-and w CARRIES)) b [::fd b])))))))))

;; the reader's recvmsg: a msghdr over the buffer's free room, and control room for up to
;; ctl-fds descriptors (they arrive with the first bytes of the frame that carries each)
(def ^:private ctl-fds 8)
(def ^:private ctl-room 64)

(defn- received-fds
  "The descriptors in a recvmsg's control data of len bytes at ctl (each SCM_RIGHTS message's)."
  [ctl len]
  (loop [off 0 acc []]
    (if (> (+ off cmsg-hdr) len)
      acc
      (let [clen (if macos? (ffi/read (+ ctl off) :uint32) (ffi/read (+ ctl off) :uint64))
            acc (if (= SCM-RIGHTS (ffi/read (+ ctl off) :int (- cmsg-hdr 4)))
                  (into acc (for [i (range (quot (- clen cmsg-hdr) 4))] (ffi/read (+ ctl off) :int (+ cmsg-hdr (* 4 i)))))
                  acc)
            step (let [a (if macos? 4 8)] (* a (quot (+ clen (dec a)) a)))]
        (if (pos? step) (recur (+ off step) acc) acc)))))

(defn- rd-fill!
  "One recvmsg into the buffer: bytes read (> 0), or :eof (EOF or a terminal error); a
  descriptor that came with them joins fds (a volatile vector), oldest first. m is the
  reader's msghdr and control room. :thread blocks in recvmsg; :fiber parks on the poller
  while the socket is empty."
  [fd ^longs st pump m fds]
  (let [lo (aget st 2) avail (- (aget st 3) lo)
        need (if (< avail 4) 4 (let [n (ffi/read (+ (aget st 0) lo) :uint32)] (if (= n END) 4 (+ 4 n))))]
    (when (or (> (+ lo need) (aget st 1)) (= (aget st 3) (aget st 1))) (rd-room! st need))
    (loop []
      (let [hi (aget st 3)
            iov (+ m 56) ctl (+ m 72)
            _ (do (ffi/write iov :uint64 (+ (aget st 0) hi) 0) (ffi/write iov :uint64 (- (aget st 1) hi) 8)
                  (ffi/write m :uint64 0 0) (ffi/write m :uint32 0 8)
                  (ffi/write m :uint64 iov 16) (if macos? (ffi/write m :int 1 24) (ffi/write m :uint64 1 24))
                  (ffi/write m :uint64 ctl 32) (if macos? (ffi/write m :uint32 ctl-room 40) (ffi/write m :uint64 ctl-room 40))
                  (ffi/write m :int 0 (if macos? 44 48)))
            [r e] (c-recvmsg fd m 0)]
        (when (pos? r)
          (let [clen (if macos? (ffi/read m :uint32 40) (ffi/read m :uint64 40))]
            (when (pos? clen) (vswap! fds into (received-fds ctl clen)))))
        (cond
          (pos? r) (do (aset st 3 (+ hi r)) r)
          (zero? r) :eof
          (native/eintr? e) (recur)
          (and (= pump :fiber) (native/eagain? e)) (do (poller/wait-ready fd :read) (recur))
          :else :eof)))))

(defn write-step
  "What a send that answered errno e does next: :retry (interrupted), :wait (the kernel cannot
  take the bytes now: EAGAIN on a non-blocking end, ENOBUFS when it is short of buffers; go on
  once the socket can take them), :dead (EPIPE, ECONNRESET: the peer is gone). Any other errno
  is a defect, thrown with it: the pump closes both halves and rethrows on its own thread. Before
  it every errno but these two read as :dead, so a busy kernel's ENOBUFS ended a live peer's end."
  [e]
  (cond (native/eintr? e) :retry
        (or (native/eagain? e) (native/enobufs? e)) :wait
        (or (= e native/epipe) (= e native/econnreset)) :dead
        :else (throw (ex-info (str "jolt.async.remote: a send failed, errno " e) {:errno e}))))

(defn- write-all!
  "Write n bytes at p. Answers true, or :dead (the peer is gone); write-step classifies a
  failed send. A :wait parks on the poller (:fiber) or polls for POLLOUT (:thread)."
  [fd p n pump]
  (loop [off 0]
    (if (= off n)
      true
      (let [[r e] (native/c-send fd (+ p off) (- n off) native/msg-nosignal)]
        (cond
          (pos? r) (recur (+ off r))
          ;; a send of n > 0 bytes that took none: the socket is done
          (zero? r) :dead
          :else
          (case (write-step e)
            :retry (recur off)
            :wait (do (if (= pump :fiber) (poller/wait-ready fd :write) (native/poll-one fd native/pollout -1)) (recur off))
            :dead :dead))))))

;; ---- an end -------------------------------------------------------------------

(def ^:private batch-bytes
  "The writer gathers values already queued on :out into one write up to this size."
  65536)

(defn- finish!
  "One pump is done; the second to finish closes the descriptor. Under the end's
  lock, with close!: once closed, the number may belong to a new socket, so nothing
  may touch it after this."
  [{:keys [fd pump ::lock ::live]}]
  (locking lock
    (when (zero? (vswap! live dec))
      ;; the poller keeps per-number state (a ready mark, a cancel): drop it, or the
      ;; next socket given this number inherits it
      (when (= pump :fiber) (poller/forget! fd))
      (close-fd! fd))))

(defn- dead!
  "The peer is gone: both halves close locally (a take drains :in, then nil)."
  [{:keys [in out close-in?]}]
  (when close-in? (a/close! in))
  (a/close! out))

(defn- reader [{:keys [fd in decode pump close-in?] :as end}]
  (let [st (rd-new)
        ;; msghdr @0 (48 bytes; 56 on Linux, whose controllen is a size_t and which has a flags word
        ;; at 48 to match), iovec @56, control @72: past the larger msghdr on both
        m (let [a (ffi/address (ffi/alloc (+ 72 ctl-room)))] (dotimes [i (+ 72 ctl-room)] (ffi/write a :uint8 0 i)) a)
        fds (volatile! [])]
    (try
      (loop []
        (let [f (rd-frame st)]
          (cond
            (identical? f ::end) (when close-in? (a/close! in))
            ;; a frame that carries a descriptor takes the oldest received: it came with the
            ;; frame's first bytes, so it is here once the frame is whole
            (vector? f) (let [d (first @fds) v (decode (second f))]
                          (vswap! fds #(vec (rest %)))
                          (a/>!! in (vary-meta v assoc ::fd d))
                          (recur))
            ;; a put to an :in its consumer closed answers false: the value is dropped
            (some? f) (do (a/>!! in (decode f)) (recur))
            :else (if (= :eof (rd-fill! fd st pump m fds))
                    (dead! end)       ; a partial frame at EOF is discarded with the peer
                    (recur)))))
      (catch Throwable t (dead! end) (throw t))
      (finally (ffi/free (aget st 0)) (ffi/free m)
               ;; descriptors no frame took are this process's to close
               (run! close-fd! @fds)
               (finish! end)))))

(defn- send-with-fd!
  "Write the n bytes at p as one sendmsg carrying descriptor d (SCM_RIGHTS: it goes with the
  first byte the kernel takes), the rest as write-all! writes them. Answers true or :dead."
  [fd p n d pump]
  (let [m (ffi/alloc (+ 56 16 cmsg-space)) mp (ffi/address m) iov (+ mp 56) ctl (+ iov 16)]
    (try
      (dotimes [i (+ 56 16 cmsg-space)] (ffi/write m :uint8 0 i))
      (ffi/write iov :uint64 p 0) (ffi/write iov :uint64 n 8)
      (if macos? (ffi/write ctl :uint32 (+ cmsg-hdr 4) 0) (ffi/write ctl :uint64 (+ cmsg-hdr 4) 0))
      (ffi/write ctl :int native/sol-socket (- cmsg-hdr 8)) (ffi/write ctl :int SCM-RIGHTS (- cmsg-hdr 4))
      (ffi/write ctl :int d cmsg-hdr)
      (ffi/write m :uint64 iov 16) (ffi/write m :int 1 24)
      (ffi/write m :uint64 ctl 32) (if macos? (ffi/write m :uint32 cmsg-space 40) (ffi/write m :uint64 cmsg-space 40))
      (loop []
        (let [[r e] (c-sendmsg fd m native/msg-nosignal)]
          (cond
            (pos? r) (if (= r n) true (write-all! fd (+ p r) (- n r) pump))
            (zero? r) :dead
            :else (case (write-step e)
                    :retry (recur)
                    :wait (do (if (= pump :fiber) (poller/wait-ready fd :write) (native/poll-one fd native/pollout -1)) (recur))
                    :dead :dead))))
      (finally (ffi/free m)))))

(defn- writer [{:keys [fd out encode pump] :as end}]
  (let [scratch (volatile! [(ffi/address (ffi/alloc start-bytes)) start-bytes])
        room! (fn [need]
                (let [[p cap] @scratch]
                  (if (<= need cap)
                    p
                    (let [nc (loop [c cap] (if (< c need) (recur (* 2 c)) c)) np (ffi/address (ffi/alloc nc))]
                      (ffi/copy p np cap) (ffi/free p) (vreset! scratch [np nc]) np))))]
    (try
      (loop [held nil]
        (let [v (or held (a/<!! out))]
          (cond
            ;; :out closed here: the end frame follows every value put before the close
            (nil? v) (let [p (room! 4)] (ffi/write p :uint32 END 0) (write-all! fd p 4 pump))
            ;; a value carrying a descriptor (::fd in its metadata) is a write of its own, the
            ;; descriptor with its frame's first bytes
            (::fd (meta v))
            (let [^bytes b (encode v) len (alength b) p (room! (+ 4 len))]
              (ffi/write p :uint32 (bit-or CARRIES len) 0)
              (ffi/write-array (+ p 4) b 0 len)
              (if (true? (send-with-fd! fd (first @scratch) (+ 4 len) (::fd (meta v)) pump)) (recur nil) (dead! end)))
            ;; this value, then whatever else is already queued, as one write, up to the next
            ;; value that carries a descriptor (held for its own)
            :else
            (let [[n held] (loop [v v off 0]
                             (let [^bytes b (encode v) len (alength b)
                                   p (room! (+ off 4 len))]
                               (ffi/write p :uint32 len off)
                               (ffi/write-array (+ p off 4) b 0 len)
                               (let [off (+ off 4 len)]
                                 (if (< off batch-bytes)
                                   (let [nv (a/poll! out)]
                                     (cond (nil? nv) [off nil]
                                           (::fd (meta nv)) [off nv]
                                           :else (recur nv off)))
                                   [off nil]))))]
              (if (true? (write-all! fd (first @scratch) n pump))
                (recur held)
                (dead! end))))))
      (catch Throwable t (dead! end) (throw t))
      (finally (ffi/free (first @scratch)) (finish! end)))))

(defn- spawn [pump f]
  (if (= pump :fiber)
    (jolt.fibers/spawn f)
    (doto (Thread. ^Runnable f) (.setDaemon true) (.start))))

(defn open
  "Make an end over connected stream socket fd, which this end then owns: it becomes
  close-on-fork, and it is closed once both directions are done (:fd is then no
  longer this end's to touch). Answers {:in ch :out ch :fd fd} (plus private keys
  close! uses).

  opts:
    :buf      buffer of :in and of :out (default 16); :in-buf and :out-buf set one
    :in       a channel to deliver into instead of a new one (a Flow in-chan, a shared
              fan-in); with :close-in? false, the peer's end and death leave it open
    :codec    {:encode :decode} over byte-arrays (default edn-codec)
    :pump     :thread (default) or :fiber"
  ([fd] (open fd {}))
  ([fd {:keys [buf in-buf out-buf in close-in? codec pump]
        :or {buf 16 close-in? true codec edn-codec pump :thread}}]
   (clofork! fd)
   (when (= pump :fiber) (fcntl fd F-SETFL (bit-or O-NONBLOCK (fcntl fd F-GETFL 0))))
   (let [end {:fd fd
              :in (or in (a/chan (or in-buf buf)))
              :out (a/chan (or out-buf buf))
              :close-in? close-in?
              :encode (:encode codec) :decode (:decode codec)
              :pump pump ::lock (Object.) ::live (volatile! 2)}]
     (spawn pump #(reader end))
     (spawn pump #(writer end))
     (select-keys end [:in :out :fd ::lock ::live]))))

(defn remote-pair
  "[end fd]: an end in this process, and the raw descriptor of the other end for the
  peer process: a child forked next inherits it (then close it here with close-fd!),
  or send-fd! hands it to a running process (close it here once that process has
  answered). The peer calls (open fd opts).
  opts: open's, plus :sock-buf (socketpair's)."
  ([] (remote-pair {}))
  ([opts]
   (let [[a b] (socketpair opts)]
     [(open a opts) b])))

(defn close!
  "Close both halves of an end: an end frame follows what was put on :out, and :in
  stops taking from the peer."
  [{:keys [in out fd ::lock ::live]}]
  ;; wake the reader while the descriptor is still this end's (see finish!)
  (locking lock (when (pos? @live) (c-shutdown fd native/shut-rd)))
  (a/close! out)
  (a/close! in)
  nil)
