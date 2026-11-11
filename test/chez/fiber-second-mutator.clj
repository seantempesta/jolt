;; A fiber is a second mutator: the single-threaded fast paths (no claim on a lazy node's
;; first force, no monitor around a ConcurrentHashMap's body) end at the first fiber, as
;; they end at the first OS thread. Each case runs in its own process (fiber-second-
;; mutator.sh) on the main carrier, where every fiber shares one OS thread, and prints
;; its name, then ok or FAIL with what it saw.
(def mode (first *command-line-args*))

(defn vthread [f] (Thread/startVirtualThread f))
(defn safely [f] (try (f) (catch Throwable e [:threw (.getName (class e)) (ex-message e)])))
(defn on-main-carrier [f] (jolt.host/main-carrier-run! f))

;; a lazy seq is realized once, whoever forces it: a second fiber forcing it while the
;; first is parked inside its body waits for that body, as a second thread does
(def lazy-runs (atom 0))
(defn lazy-twice []
  (on-main-carrier
    (fn []
      (let [s (lazy-seq (swap! lazy-runs inc) (Thread/sleep 20) [1 2])
            r1 (promise) r2 (promise)
            a (vthread #(deliver r1 (safely (fn [] (vec s)))))
            b (vthread #(deliver r2 (safely (fn [] (vec s)))))]
        (.join a) (.join b)
        [@lazy-runs @r1 @r2]))))

;; a seq over an iterator takes each element once: two fibers walking it see the same seq
(defn iterator-twice []
  (on-main-carrier
    (fn []
      (let [n (atom 0)
            it (reify java.util.Iterator
                 (hasNext [_] (< @n 4))
                 (next [_] (Thread/sleep 5) (swap! n inc)))
            s (iterator-seq it)
            r1 (promise) r2 (promise)
            a (vthread #(deliver r1 (safely (fn [] (vec s)))))
            b (vthread #(deliver r2 (safely (fn [] (vec s)))))]
        (.join a) (.join b)
        [@r1 @r2]))))

;; ConcurrentHashMap.compute is atomic per key: no update is lost, and no fiber is left
;; waiting, when the function parks
(defn chm-compute []
  (on-main-carrier
    (fn []
      (let [m (java.util.concurrent.ConcurrentHashMap.)
            errs (atom [])
            ts (mapv (fn [_] (vthread #(try (.compute m :k (reify java.util.function.BiFunction
                                                             (apply [_ _ v] (let [n (or v 0)] (Thread/sleep 2) (inc n)))))
                                            (catch Throwable e (swap! errs conj (.getName (class e)))))))
                     (range 8))]
        (run! #(.join %) ts)
        [(.get m :k) @errs]))))

;; a forced node costs the same bytes after the first fiber as before it: the claim the safe
;; path takes allocates nothing (seq.ss force-claimer). Measured in one process, the walk's
;; bytes a node before any fiber exists and after one has, for a lazy seq node (the claim on
;; its lock) and an iterate cell (the claim in its tail word). The claim cost a node 128
;; bytes (a lazy seq node) and 144 (an iterate cell) before the claim was made once an owner.
(defn- bytes-now [] (jolt.host/scheme-eval-string "(sstats-bytes (statistics))"))
(defn- hand [n] (lazy-seq (when (pos? n) (cons n (hand (dec n))))))
(def walks {:lazy-seq #(count (hand 20000)) :iterate #(count (take 20000 (iterate inc 0)))})
(defn- walk-bytes [f] (f) (apply min (for [_ (range 3)] (let [b0 (bytes-now)] (f) (- (bytes-now) b0)))))
(defn- node-bytes [] (into {} (for [[k f] walks] [k (quot (walk-bytes f) 20000)])))
(defn lazy-alloc []
  (let [fast (node-bytes)
        _ (.join (vthread (fn [] 1)))
        safe (node-bytes)
        extra (into {} (for [[k b] safe] [k (- b (fast k))]))]
    (if (every? #(<= % 2) (vals extra)) :same [:extra-bytes-a-node extra :fast fast :safe safe])))

(def cases
  {"lazy-alloc"     [lazy-alloc     :same]
   "lazy-twice"     [lazy-twice     [1 [1 2] [1 2]]]
   "iterator-twice" [iterator-twice [[1 2 3 4] [1 2 3 4]]]
   "chm-compute"    [chm-compute    [8 []]]})

(let [[f expected] (cases mode)
      got (safely f)]
  (println (str "fiber-second-mutator " mode ": " (if (= got expected) "ok" (str "FAIL " (pr-str got) " expected " (pr-str expected))))))
