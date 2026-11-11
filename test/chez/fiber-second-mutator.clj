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

;; --- the claim's edges: each owner's claims are one stack and one winder (seq.ss force-claimer)
(require '[jolt.scheme :as scm])
(scm/eval-string "(define fsm-node-claimed? (lambda (x) (if (jolt-lazyseq-lock x) #t #f)))")
(scm/eval-string "(define fsm-cell-claimed? (lambda (c) (tail-claim? (cseq-tail c))))")
(def node-claimed? (scm/proc "fsm-node-claimed?"))
(def cell-claimed? (scm/proc "fsm-cell-claimed?"))
(defn- claimed-any? [nodes] (boolean (some node-claimed? nodes)))
;; F on a new fiber, its answer within 5s, or :timed-out
(defn- on-fiber [f] (let [p (promise)] (vthread #(deliver p (safely f))) (deref p 5000 :timed-out)))

;; a body that forces another, 40 deep: past the 8 depths a claimer starts with, so its
;; stack grows while the outer claims are held; afterwards none is held
(defn deep-nest []
  (on-main-carrier
    (fn []
      (let [nodes (reduce (fn [acc i] (cons (lazy-seq (cons i (seq (first acc)))) acc))
                          (list (lazy-seq nil)) (range 39 -1 -1))
            nodes (vec (butlast nodes))]
        [(vec (first nodes)) (claimed-any? nodes) (on-fiber #(count (mapv seq nodes)))]))))

;; three bodies nested, the innermost raising on its first run: the raise reaches the caller,
;; no claim is left, and a second fiber's force reruns each body once more
(def rn-runs (atom [0 0 0]))
(declare rn1 rn2)
(def rn0 (lazy-seq (swap! rn-runs update 0 inc) (cons 0 (seq rn1))))
(def rn1 (lazy-seq (swap! rn-runs update 1 inc) (cons 1 (seq rn2))))
(def rn2 (lazy-seq (swap! rn-runs update 2 inc)
                   (when (= 1 (get @rn-runs 2)) (throw (ex-info "boom" {})))
                   (list 2)))
(defn raise-nested []
  (on-main-carrier
    (fn []
      [(safely #(vec rn0)) (claimed-any? [rn0 rn1 rn2]) (on-fiber #(vec rn0)) @rn-runs])))

;; fiber A parks inside a body and is killed (or interrupted) there: the unwind releases its
;; claim, and fiber B's force reruns the body rather than waiting on A for good
(def pk-runs (atom 0))
(def pk (lazy-seq (when (= 1 (swap! pk-runs inc)) (Thread/sleep 5000)) [1 2]))
(defn- park-then [stop!]
  (on-main-carrier
    (fn []
      (let [fa (promise) ra (promise)]
        (vthread #(do (deliver fa (jolt.host/current-fiber)) (deliver ra (safely (fn [] (vec pk))))))
        (loop [] (when (zero? @pk-runs) (Thread/sleep 1) (recur)))
        (stop! @fa (Exception. "stop"))
        (let [r (deref ra 1000 :never)]
          ;; a kill escapes past A's try (only finally runs), so A never delivers
          [(if (vector? r) (first r) r) (node-claimed? pk) (on-fiber #(vec pk)) @pk-runs])))))
(defn park-then-kill [] (park-then jolt.host/fiber-kill!))
(defn park-then-interrupt [] (park-then jolt.host/fiber-interrupt!))

;; two fibers each walk a chain of 500 nodes whose bodies park, forcing 3 nodes of the other's
;; chain at every step: every body runs once and every value is right
(def il-runs (atom {}))
(defn- il-chain [c n]
  (loop [j (dec n) nxt nil acc ()]
    (if (neg? j)
      (vec acc)
      (let [node (lazy-seq (swap! il-runs update [c j] (fnil inc 0)) (Thread/sleep 0) (cons [c j] nxt))]
        (recur (dec j) node (cons node acc))))))
(defn interleave-chains []
  (on-main-carrier
    (fn []
      (let [n 500
            a (il-chain :a n) b (il-chain :b n)
            walk (fn [mine other]
                   (loop [s (first mine) j 0 seen []]
                     (if-let [x (seq s)]
                       (do (doseq [k [j (+ j 7) (+ j 13)]] (when (< k n) (first (nth other k))))
                           (recur (rest x) (inc j) (conj seen (first x))))
                       seen)))
            pa (promise) pb (promise)]
        (vthread #(deliver pa (safely (fn [] (walk a b)))))
        (vthread #(deliver pb (safely (fn [] (walk b a)))))
        (let [wa (deref pa 20000 :timed-out) wb (deref pb 20000 :timed-out)]
          [(= wa (mapv #(vector :a %) (range n))) (= wb (mapv #(vector :b %) (range n)))
           (count @il-runs) (set (vals @il-runs)) (claimed-any? (concat a b))])))))

;; the cell path (an iterate cell's tail word is the claim): f raises at x = 3 on its first
;; call, inside two maps; the cell's word is pending again, and a second fiber reruns it
(def cr-calls (atom 0))
(defn- cr-f [x] (when (and (= x 3) (= 1 (swap! cr-calls inc))) (throw (ex-info "boom" {}))) (inc x))
(defn cell-raise []
  (on-main-carrier
    (fn []
      (let [s (iterate cr-f 0)
            m (map inc (map inc s))]
        [(first (safely #(nth m 5))) (cell-claimed? (nthrest s 3)) (on-fiber #(nth m 5)) @cr-calls]))))

;; the main thread (no fiber current) holds a claim while its body drives the main carrier,
;; whose fibers force other nodes on the same OS thread
(defn thread-and-fibers []
  (.join (doto (Thread. (fn [])) .start))              ; a second thread: the safe path from here
  (let [y (lazy-seq (Thread/sleep 1) [:y]) z (lazy-seq (Thread/sleep 1) [:z])
        x (lazy-seq (jolt.host/main-carrier-run!
                      (fn []
                        (let [p (promise)]
                          (vthread #(do (vec y) (vec z) (deliver p :delivered)))
                          [(deref p 5000 :timed-out) (vec y) (vec z)]))))]
    [(vec x) (claimed-any? [x y z])]))

(def cases
  {"lazy-alloc"     [lazy-alloc     :same]
   "deep-nest"      [deep-nest      [(vec (range 40)) false 40]]
   "raise-nested"   [raise-nested   [[:threw "clojure.lang.ExceptionInfo" "boom"] false [0 1 2] [2 2 2]]]
   "park-then-kill" [park-then-kill [:never false [1 2] 2]]
   "park-then-interrupt" [park-then-interrupt [:threw false [1 2] 2]]
   "interleave"     [interleave-chains [true true 1000 #{1} false]]
   "cell-raise"     [cell-raise     [:threw false 7 2]]
   "thread-and-fibers" [thread-and-fibers [[:delivered [:y] [:z]] false]]
   "lazy-twice"     [lazy-twice     [1 [1 2] [1 2]]]
   "iterator-twice" [iterator-twice [[1 2 3 4] [1 2 3 4]]]
   "chm-compute"    [chm-compute    [8 []]]})

(let [[f expected] (cases mode)
      got (safely f)]
  (println (str "fiber-second-mutator " mode ": " (if (= got expected) "ok" (str "FAIL " (pr-str got) " expected " (pr-str expected))))))
