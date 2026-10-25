;; jolt.host/fork-alone: fork(2) safe by construction (pthread_atfork's rule). fork copies only
;; the calling thread, so a lock another thread held stays locked in the child forever (Seon's
;; full run: a child in S_condition_wait -> mutexwait). A thread the program started may hold any
;; lock of the program's, so the fork beside one is refused (-3); the runtime's own threads hold
;; only the runtime's locks, which the forking thread takes first, then Chez's ((cs)fork_alone:
;; the thread and allocation mutexes), so a fork while the timer thread fires timeouts and
;; collections are requested gives a child whose timers, go blocks and collections work.
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))
(def wait-exit
  (scm/eval-string
    "(let ([waitpid (foreign-procedure \"waitpid\" (int uptr int) int)])
       (lambda (pid)
         (let ([b (foreign-alloc 4)])
           (waitpid pid b 0)
           (let ([s (foreign-ref 'int b 0)]) (foreign-free b) s))))"))
(def collect! (scm/eval-string "(lambda () (collect))"))
(def exit-now (scm/eval-string "(let ([e (foreign-procedure \"_exit\" (int) void)]) (lambda (n) (e n)))"))

(defn forked
  "The wait status of a child fork-alone makes that runs (f): exit 0 when it answers true, 1
  else, 2 when it threw; or [:refused code]."
  [f]
  (let [pid (jolt.host/fork-alone)]
    (cond (zero? pid) (exit-now (try (if (true? (f)) 0 1) (catch Throwable t (binding [*out* *err*] (println "CHILD THREW" (ex-message t) (pr-str (ex-data t))) (flush)) 2)))
          (neg? pid) [:refused pid]
          :else (wait-exit pid))))

(defn child-works
  "In the child: a collection runs (first: Chez collects on demand only while one thread is
  active, and the go block below starts the child's carriers), a timeout fires, and a go block
  parks and resumes."
  []
  (and (do (collect!) true)
       (nil? (a/<!! (a/timeout 1)))
       (= 41 (let [c (a/chan) r (a/go (a/<! c))] (a/>!! c 41) (a/<!! r)))))

;; first, before anything has collected: the shutdown watcher sits in sigwait, a collect-safe call,
;; and counts as active until something converts it (Chez's native_in_native); the fork converts
;; it, so this goes through rather than refused (-2) until the first collection
(check-eq "a fork before the first collection, beside the shutdown watcher, works" (forked (fn [] true)) 0)

;; refused beside a thread the program started, while it lives
(let [go (promise) t (Thread. #(deref go))]
  (.start t)
  (check-eq "a fork beside a live Thread is refused (-3)" (forked (constantly true)) [:refused -3])
  (check-eq "the program's threads are counted" (pos? (jolt.host/user-threads)) true)
  (deliver go true)
  (.join t))

;; 50 forks with the timer thread firing: each round arms timeouts due around the fork and
;; allocates enough to request collections, then forks; every child must work and exit 0
(dotimes [i 50]
  (let [ts (doall (for [ms [0 1 2]] (a/timeout ms)))]
    (dotimes [_ 200] (vec (range 1000)))
    (check-eq (str "fork " i ": the child's timers, go blocks and collections work") (forked child-works) 0)
    (doseq [t ts] (a/<!! t))))

;; the carriers started (a go block ran): idle, each in its loop's wait with its queue empty, they
;; hold no lock, so 20 more forks with timers firing go through, and every child works
(check-eq "a go block runs on the carriers" (a/<!! (a/go 7)) 7)
(dotimes [i 20]
  (let [ts (doall (for [ms [0 1 2]] (a/timeout ms)))]
    (check-eq (str "fork " i " beside idle carriers: the child works") (forked child-works) 0)
    (doseq [t ts] (a/<!! t))))
;; the io poller (a runtime thread, jolt.host/runtime-thread) runs once a fiber waits on a socket:
;; in kevent it holds no lock, so forks beside it go through, and each child, whose poller the
;; fork left behind (after-fork! resets the table), starts its own and reads a socket from a fiber
(defn socket-read
  "A fiber's read of one byte a socket peer writes, true when it came (the io poller wakes it)."
  []
  (let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
        c (java.net.Socket. "127.0.0.1" (.getLocalPort ss)) s (.accept ss)
        o (binding [a/*go-backend* :fiber] (a/go (.read (.getInputStream c) (byte-array 1) 0 1)))]
    (Thread/sleep 5)
    (.write (.getOutputStream s) (byte-array [120]) 0 1)
    (let [n (a/<!! o)] (.close c) (.close s) (.close ss) (= 1 n))))
(check-eq "a fiber reads a socket" (socket-read) true)
(check-eq "the io poller runs, and is not a program thread"
          [(boolean (some #{"jolt-io-poller"} (map #(.getName ^Thread %) (keys (Thread/getAllStackTraces))))) (jolt.host/user-threads)]
          [true 0])
(dotimes [i 20]
  (check-eq (str "fork " i " beside the io poller: the child works and reads a socket")
            (forked #(and (child-works) (socket-read))) 0))
;; a carrier running a go block may hold any lock of the program's: refused (-4)
(let [stop (atom false) started (promise)
      busy (a/go (deliver started true) (loop [] (when-not @stop (recur))) :stopped)]
  @started
  (check-eq "a fork beside a busy carrier is refused (-4)" (forked (constantly true)) [:refused -4])
  (reset! stop true)
  (check-eq "the busy go block ends" (a/<!! busy) :stopped))

(if (empty? @failures)
  (do (println "FORK-ALONE-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
