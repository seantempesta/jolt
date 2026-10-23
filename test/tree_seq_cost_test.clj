;; tree-seq costs its nodes: walking a tree allocates a bounded number of bytes a
;; node visited, measured as the slope between two sizes (n and 10n nodes) of one
;; shape, the least of several runs (bytes are deterministic where time is not).
;;
;; Clojure's definition (a lazy-seq per node, its children spliced by mapcat) cost
;; ~1.1 KB a node here: mapcat's apply concat built a concat cell, a lazy seq and a
;; map cell per child on every level. Seon walks every form it imports with
;; tree-seq. The walk keeps a list of the sibling seqs still to visit instead, so a
;; node costs its lazy cell, its cons and one push. Same order (pre-order), and as
;; lazy: nothing runs at construction, a node's branch? and children run when the
;; node is reached, and its rest when the walk passes it.

(ns tree-seq-cost-test)

(defn- bytes-now [] (jolt.host/scheme-eval-string "(sstats-bytes (statistics))"))
(defn- walk-bytes [x]
  (count (tree-seq coll? seq x))
  (apply min (for [_ (range 5)] (let [b0 (bytes-now)] (count (tree-seq coll? seq x)) (- (bytes-now) b0)))))
(defn- nodes [x] (count (tree-seq coll? seq x)))

(def shapes
  "Each shape of about n nodes, and its ceiling in bytes a node visited."
  [["nested lists, three leaves a level" (fn [n] (reduce (fn [x i] (list 'f i :k x)) 'x (range (quot n 4)))) 320]
   ["flat vector of leaves" (fn [n] (vec (range n))) 320]
   ["a form as read: lists of lists" (fn [n] (vec (repeat (quot n 4) '(str :k "s")))) 320]])

(def failures (atom 0))
(doseq [[what f ceiling] shapes]
  (let [a (f 1000) b (f 10000)
        s (/ (double (- (walk-bytes b) (walk-bytes a))) (- (nodes b) (nodes a)))]
    (println (format "%-40s %7.0f B a node (ceiling %d)" what s ceiling))
    (when (> s ceiling) (swap! failures inc) (println "FAIL:" what (long s) "B a node, past" ceiling))))

;; the same walk as Clojure's, node for node, laziness included
(let [ref-tree-seq (fn [branch? children root]
                     (let [walk (fn walk [node] (lazy-seq (cons node (when (branch? node) (mapcat walk (children node))))))]
                       (walk root)))
      trees [[] [1] [1 [2] 3] '(a (b (c)) d) {:a [1 {:b 2}]} [[[[]]] [] [1 [2 [3 [4]]]]]]
      calls (atom 0)]
  (doseq [t trees]
    (when-not (= (ref-tree-seq coll? seq t) (tree-seq coll? seq t))
      (swap! failures inc) (println "FAIL: order differs on" (pr-str t))))
  (tree-seq (fn [x] (swap! calls inc) true) seq [1 [2]])
  (when-not (zero? @calls) (swap! failures inc) (println "FAIL: branch? ran at construction"))
  (first (tree-seq (fn [x] (swap! calls inc) true) seq [1 [2]]))
  (when-not (= 1 @calls) (swap! failures inc) (println "FAIL: first ran branch?" @calls "times, not once")))
(if (zero? @failures)
  (println "tree-seq-cost: OK")
  (do (println "tree-seq-cost:" @failures "FAILED") (System/exit 1)))
