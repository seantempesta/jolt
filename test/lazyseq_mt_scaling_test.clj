;; Lazy realization must cost the same whether or not a thread has ever existed.
;;
;; A lazy cell (host/chez/lazy-bridge.ss) and a seq cell with a deferred tail
;; (host/chez/seq.ss) publish their result through ONE field: the tail slot holds
;; the thunk until it is forced and the seq after, told apart by type. A reader
;; therefore never locks: it loads the slot and either has its answer or takes the
;; slow path. Exclusion is needed only to run a thunk once, and the mutex for that
;; is borrowed from a pool for the duration of the force, so a program pays no
;; per-cell mutex — a Chez mutex is a finalized object the collector has to visit,
;; and paying one per lazy element made every collection ~120x dearer the moment a
;; single thread had been forked, for the rest of the process.
;;
;; Two assertions:
;;
;;   ONCE-ONLY — eight threads walk the same unrealized seqs at once. Every
;;   producer runs exactly once per element, every walker sees the same values,
;;   and a producer that throws throws the SAME failure to every walker. This is
;;   the contract the lock-free read path must not weaken.
;;
;;   SCALING — one workload, run before any thread exists and again after one
;;   has existed, in ONE process, judged by COUNTING the mutexes it allocates
;;   (jolt.host/mutex-allocations, host/chez/scheme-adapter-runtime.ss). That count is exactly what
;;   the guarded regression changes: a mutex per lazy cell once a thread has
;;   existed is ~600,000 for this workload (one per claimed force), and the claim
;;   design allocates none in either arm. The count is the cause of the old slowdown (a finalized object
;;   per cell, each visited by every collection), so judging it judges the
;;   slowdown without a clock: it reads the same on an idle machine and a loaded
;;   one, where a timed ratio (~1.75 for the claim, ~5 for a mutex per cell)
;;   drifted toward its ceiling with the runner's neighbours. The time of each
;;   arm is still printed, as information.

(ns lazyseq-mt-scaling-test)

(def ^:private walkers 8)
(def ^:private n 20000)
;; Mutexes the scaling workload may allocate in an arm. The claim design
;; allocates none; a few would be a runtime structure made once (a pool growing,
;; say), and the regression is one per cell, ~600,000 per run.
(def ^:private max-mutexes 16)

(defn- fail [msg]
  (println (str "FAIL lazyseq-mt-scaling: " msg))
  (System/exit 1))

(defn- gen
  "A user lazy-seq producer: one thunk per element, counted."
  [calls i]
  (lazy-seq
    (swap! calls inc)
    (when (< i n) (cons i (gen calls (inc i))))))

(defn- run-walkers
  "Start `walkers` threads that each run f, return their results in order."
  [f]
  (let [fs (doall (repeatedly walkers #(future (f))))]
    (mapv deref fs)))

(defn- check-once-only []
  ;; a user lazy-seq chain: every cell forced by 8 racing threads
  (let [calls (atom 0)
        s (gen calls 0)
        sums (run-walkers #(reduce + 0 s))]
    (when-not (apply = sums) (fail (str "walkers disagree over a lazy-seq chain: " sums)))
    (when-not (= (inc n) @calls)
      (fail (str "lazy-seq bodies ran " @calls " times for " (inc n) " cells — a thunk ran twice"))))
  ;; a native producer chain (map over an unchunked source): a cseq tail thunk per element
  (let [calls (atom 0)
        s (map (fn [x] (swap! calls inc) x) (take n (iterate inc 0)))
        sums (run-walkers #(reduce + 0 s))]
    (when-not (apply = sums) (fail (str "walkers disagree over a map chain: " sums)))
    (when-not (= n @calls)
      (fail (str "map's fn ran " @calls " times for " n " elements — a tail thunk ran twice"))))
  ;; a chunked source: the vector-backed tails are computed, not run, and must
  ;; still agree
  (let [s (map inc (vec (range n)))
        sums (run-walkers #(reduce + 0 s))]
    (when-not (apply = sums) (fail (str "walkers disagree over a chunked chain: " sums))))
  ;; producers that call no user code run claim-free (seq.ss cell-run-pure!): 8 walkers
  ;; race the same unforced char seq, range and cycle, agree on every value, and
  ;; leave one shared tail on each cell
  (let [str-s (seq (apply str (repeat n \x)))
        rng (seq (range 0 (* 2 n) 2))
        cyc (take n (cycle [1 2 3]))]
    (doseq [[nm s] [["str seq" str-s] ["range" rng] ["cycle" cyc]]]
      (let [sums (run-walkers #(reduce + 0 (map hash s)))]
        (when-not (apply = sums) (fail (str "walkers disagree over a " nm ": " sums)))
        (when-not (identical? (next (next s)) (next (next s)))
          (fail (str "a " nm " cell's tail is not one shared cell after the race"))))))
  ;; a failing producer: every racing walker gets the exception, none hangs on
  ;; the claim the failed force leaves behind, and none sees an empty seq. The
  ;; body runs again for each force, as the reference's LazySeq keeps fn until
  ;; invoke returns (a ^:once body's captured locals are cleared by then, so this
  ;; one captures nothing and fails the same way every time).
  (let [s (lazy-seq (throw (ex-info "boom" {:once true})))
        msgs (run-walkers #(try (doall s) :no-throw
                                (catch clojure.lang.ExceptionInfo e (ex-data e))))]
    (when-not (every? #(= {:once true} %) msgs)
      (fail (str "a failing lazy-seq did not fail every walker the same way: " msgs))))
  (println "lazyseq-mt-scaling once-only: 8 racing walkers, every producer ran once, a failure reached all"))

(defn- work []
  ;; allocation-heavy lazy walking: a few lazy cells per iteration, many iterations
  (loop [i 0 acc 0]
    (if (< i 300000)
      (recur (inc i) (+ acc (count (vec (map inc (take 3 (iterate inc i)))))))
      acc)))

;; Each arm: the mutexes allocated (the judge) and the thread's CPU time (shown).
(def ^:private cpu-bean (java.lang.management.ManagementFactory/getThreadMXBean))

(defn- arm []
  (System/gc)
  (let [m0 (jolt.host/mutex-allocations)
        c0 (.getCurrentThreadCpuTime cpu-bean)]
    (work)
    [(- (jolt.host/mutex-allocations) m0)
     (/ (- (.getCurrentThreadCpuTime cpu-bean) c0) 1000000.0)]))

(def ^:private runs 3)
(defn- arms [] (vec (repeatedly runs arm)))

(defn- check-scaling []
  (work)                                                ; warm
  (let [before (arms)
        t (Thread. (fn [] nil))]
    (.start t) (.join t)                               ; a thread has EXISTED; it need not be alive
    (let [after (arms)
          cpu (fn [rs] (reduce min (map second rs)))]
      (println (format (str "lazyseq-mt-scaling: mutexes allocated per arm %s before any thread, %s after "
                            "one existed (limit %d); CPU %.0fms -> %.0fms (ratio %.2f, not judged)")
                       (mapv first before) (mapv first after) max-mutexes
                       (cpu before) (cpu after) (/ (cpu after) (cpu before))))
      (when-let [bad (seq (filter #(> % max-mutexes) (map first (concat before after))))]
        (fail (str "lazy realization allocates mutexes (" (first bad) " in one run of the workload) — "
                   "a mutex is being allocated per lazy cell again (host/chez/seq.ss seq-more / "
                   "host/chez/lazy-bridge.ss force-lazyseq)."))))))

(defn -main [& _]
  ;; scaling first: it needs the single-threaded arm, and once-only forks threads
  (check-scaling)
  (check-once-only)
  (println "lazyseq-mt-scaling: passed")
  ;; the walkers ran on futures; let the pool go so the process exits now
  ;; rather than after the agent pools' idle linger, as on the JVM
  (shutdown-agents))

(-main)
