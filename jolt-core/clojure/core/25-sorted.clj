;;   Portions of this file are from ClojureScript's src/main/cljs/cljs/core.cljs,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

;; clojure.core — sorted collections tier.
;;
;; A sorted-map / sorted-set is a tagged host table
;;   {:jolt/type :jolt/sorted-map|:jolt/sorted-set
;;    :tree      RB-NODE | nil   ; a red-black tree, comparator-ordered
;;    :cnt       N               ; element count (O(1))
;;    :cmp       FN-or-nil       ; 3-way comparator; nil = natural order (compare)
;;    :ops       {op-kw fn}}     ; this tier's implementations, attached to the value
;;
;; The tree is a left-leaning-free red-black tree — Rich Hickey's algorithm,
;; ported from the ClojureScript PersistentTreeMap (cljs.core: tree-map-add /
;; balance-left / balance-right / tree-map-append / balance-*-del). assoc / get /
;; dissoc / contains are O(log n). cljs uses BlackNode/RedNode
;; deftypes, but this tier loads before 30-macros (no deftype), so a node is a
;; plain vector [color k v left right] (color :red/:black; left/right node|nil)
;; and the methods become functions — the algorithm is identical.
;;
;; A sorted-SET stores its elements as keys with a nil value; its ops project the
;; key. ALL the semantics live here in Clojure; the host keeps only its
;; dispatch branches (conj/assoc/get/seq/count/…), each a one-line call through
;; the value's own :ops table, so the ops travel WITH the value (correct across
;; contexts, forks, and AOT images). The wrapper is minted/read through the host
;; value primitives jolt.host/tagged-table + ref-put! + ref-get.

;; Raw field read on the wrapper (host primitive). Plain `get` on a sorted coll
;; IS the comparator lookup — it dispatches back into these ops, so reading
;; :tree/:cmp/:ops with it would recurse forever.
(defn- sfield [sc k] (jolt.host/ref-get sc k))

;; Clojure's fn->comparator: a comparator fn may return a number (3-way) or a
;; boolean less-than predicate. The value may also be a reify/deftype Comparator,
;; which is not IFn — __comparator-fn is the shared seam that turns either into a
;; 2-arg compare fn.
(defn- fn->cmp [f]
  (let [f (__comparator-fn f)]
    (fn [a b]
      (let [r (f a b)]
        (if (number? r)
          r
          (if r -1 (if (f b a) 1 0)))))))

(defn- the-cmp [sc] (or (sfield sc :cmp) compare))

;; --- red-black tree nodes: [color key val left right] -----------------------
(defn- nd-key [n] (nth n 1))
(defn- nd-val [n] (nth n 2))
(defn- nd-left [n] (nth n 3))
(defn- nd-right [n] (nth n 4))
(defn- red? [n] (and n (identical? :red (nth n 0))))
(defn- black? [n] (and n (identical? :black (nth n 0))))
(defn- mk-red [k v l r] [:red k v l r])
(defn- mk-black [k v l r] [:black k v l r])
;; BlackNode.blacken = self; RedNode.blacken = a black copy.
(defn- blacken [n] (if (red? n) [:black (nd-key n) (nd-val n) (nd-left n) (nd-right n)] n))
;; BlackNode.redden = a red copy; RedNode.redden = invariant violation (never hit
;; on the paths that call it: redden is only applied to a known-black node).
(defn- redden [n] [:red (nd-key n) (nd-val n) (nd-left n) (nd-right n)])
;; replace a node's key/val/children KEEPING its color.
(defn- replace-node [n k v l r] (if (red? n) (mk-red k v l r) (mk-black k v l r)))

;; --- insert balancing (the RedNode/BlackNode .balance-left/.balance-right) ---
(defn- ins-balance-left [ins parent]
  (if (red? ins)
    (let [l (nd-left ins) r (nd-right ins)]
      (cond
        (red? l) (mk-red (nd-key ins) (nd-val ins)
                         (blacken l)
                         (mk-black (nd-key parent) (nd-val parent) r (nd-right parent)))
        (red? r) (mk-red (nd-key r) (nd-val r)
                         (mk-black (nd-key ins) (nd-val ins) l (nd-left r))
                         (mk-black (nd-key parent) (nd-val parent) (nd-right r) (nd-right parent)))
        :else (mk-black (nd-key parent) (nd-val parent) ins (nd-right parent))))
    (mk-black (nd-key parent) (nd-val parent) ins (nd-right parent))))

(defn- ins-balance-right [ins parent]
  (if (red? ins)
    (let [l (nd-left ins) r (nd-right ins)]
      (cond
        (red? r) (mk-red (nd-key ins) (nd-val ins)
                         (mk-black (nd-key parent) (nd-val parent) (nd-left parent) l)
                         (blacken r))
        (red? l) (mk-red (nd-key l) (nd-val l)
                         (mk-black (nd-key parent) (nd-val parent) (nd-left parent) (nd-left l))
                         (mk-black (nd-key ins) (nd-val ins) (nd-right l) r))
        :else (mk-black (nd-key parent) (nd-val parent) (nd-left parent) ins)))
    (mk-black (nd-key parent) (nd-val parent) (nd-left parent) ins)))

;; node .add-left / .add-right (parent gains a new left/right subtree `ins`)
(defn- add-left [parent ins]
  (if (red? parent)
    (mk-red (nd-key parent) (nd-val parent) ins (nd-right parent))
    (ins-balance-left ins parent)))
(defn- add-right [parent ins]
  (if (red? parent)
    (mk-red (nd-key parent) (nd-val parent) (nd-left parent) ins)
    (ins-balance-right ins parent)))

;; insert k/v into tree in ONE walk: the new tree, or nil when k is already
;; present, with that node left in the `found` volatile (PersistentTreeMap.add
;; answers null and fills its found box the same way). The callers used to look
;; the key up first and then insert, which walked the tree twice for every fresh
;; key -- twice the comparator calls of the reference, measured exactly (24602
;; against 12301 for 1024 ascending keys); the corpus pins the count now.
(defn- tree-ins [cmp tree k v found]
  (if (nil? tree)
    (mk-red k v nil nil)
    (let [c (cmp k (nd-key tree))]
      (cond
        (zero? c) (do (vreset! found tree) nil)
        (neg? c) (let [ins (tree-ins cmp (nd-left tree) k v found)]
                   (if (nil? ins) nil (add-left tree ins)))
        :else (let [ins (tree-ins cmp (nd-right tree) k v found)]
                (if (nil? ins) nil (add-right tree ins)))))))

;; replace the value at an existing key, keeping the tree structure (and the
;; first-inserted key, like Clojure's PersistentTreeMap).
(defn- tree-replace [cmp tree k v]
  (let [c (cmp k (nd-key tree))]
    (cond
      (zero? c) (replace-node tree (nd-key tree) v (nd-left tree) (nd-right tree))
      (neg? c)  (replace-node tree (nd-key tree) (nd-val tree) (tree-replace cmp (nd-left tree) k v) (nd-right tree))
      :else     (replace-node tree (nd-key tree) (nd-val tree) (nd-left tree) (tree-replace cmp (nd-right tree) k v)))))

(defn- tree-lookup [tree cmp k]
  (loop [t tree]
    (if (nil? t)
      nil
      (let [c (cmp k (nd-key t))]
        (cond (zero? c) t
              (neg? c) (recur (nd-left t))
              :else (recur (nd-right t)))))))

;; --- delete balancing (cljs standalone balance-left / balance-right / *-del) -
(defn- balance-left [k v ins right]
  (if (red? ins)
    (let [il (nd-left ins) ir (nd-right ins)]
      (cond
        (red? il) (mk-red (nd-key ins) (nd-val ins) (blacken il) (mk-black k v ir right))
        (red? ir) (mk-red (nd-key ir) (nd-val ir)
                          (mk-black (nd-key ins) (nd-val ins) il (nd-left ir))
                          (mk-black k v (nd-right ir) right))
        :else (mk-black k v ins right)))
    (mk-black k v ins right)))

(defn- balance-right [k v left ins]
  (if (red? ins)
    (let [il (nd-left ins) ir (nd-right ins)]
      (cond
        (red? ir) (mk-red (nd-key ins) (nd-val ins) (mk-black k v left il) (blacken ir))
        (red? il) (mk-red (nd-key il) (nd-val il)
                          (mk-black k v left (nd-left il))
                          (mk-black (nd-key ins) (nd-val ins) (nd-right il) ir))
        :else (mk-black k v left ins)))
    (mk-black k v left ins)))

(defn- balance-left-del [k v del right]
  (cond
    (red? del) (mk-red k v (blacken del) right)
    (black? right) (balance-right k v del (redden right))
    (and (red? right) (black? (nd-left right)))
      (mk-red (nd-key (nd-left right)) (nd-val (nd-left right))
              (mk-black k v del (nd-left (nd-left right)))
              (balance-right (nd-key right) (nd-val right) (nd-right (nd-left right)) (redden (nd-right right))))
    :else (throw (ex-info "red-black tree invariant violation" {}))))

(defn- balance-right-del [k v left del]
  (cond
    (red? del) (mk-red k v left (blacken del))
    (black? left) (balance-left k v (redden left) del)
    (and (red? left) (black? (nd-right left)))
      (mk-red (nd-key (nd-right left)) (nd-val (nd-right left))
              (balance-left (nd-key left) (nd-val left) (redden (nd-left left)) (nd-left (nd-right left)))
              (mk-black k v (nd-right (nd-right left)) del))
    :else (throw (ex-info "red-black tree invariant violation" {}))))

;; merge two subtrees (the children of a removed node)
(defn- tree-append [left right]
  (cond
    (nil? left) right
    (nil? right) left
    (red? left)
      (if (red? right)
        (let [app (tree-append (nd-right left) (nd-left right))]
          (if (red? app)
            (mk-red (nd-key app) (nd-val app)
                    (mk-red (nd-key left) (nd-val left) (nd-left left) (nd-left app))
                    (mk-red (nd-key right) (nd-val right) (nd-right app) (nd-right right)))
            (mk-red (nd-key left) (nd-val left) (nd-left left)
                    (mk-red (nd-key right) (nd-val right) app (nd-right right)))))
        (mk-red (nd-key left) (nd-val left) (nd-left left) (tree-append (nd-right left) right)))
    (red? right)
      (mk-red (nd-key right) (nd-val right) (tree-append left (nd-left right)) (nd-right right))
    :else
      (let [app (tree-append (nd-right left) (nd-left right))]
        (if (red? app)
          (mk-red (nd-key app) (nd-val app)
                  (mk-black (nd-key left) (nd-val left) (nd-left left) (nd-left app))
                  (mk-black (nd-key right) (nd-val right) (nd-right app) (nd-right right)))
          (balance-left-del (nd-key left) (nd-val left) (nd-left left)
                            (mk-black (nd-key right) (nd-val right) app (nd-right right)))))))

;; remove k from tree, assuming k IS present (the caller checks).
(defn- tree-del [cmp tree k]
  (let [c (cmp k (nd-key tree))]
    (cond
      (zero? c) (tree-append (nd-left tree) (nd-right tree))
      (neg? c) (let [del (tree-del cmp (nd-left tree) k)]
                 (if (black? (nd-left tree))
                   (balance-left-del (nd-key tree) (nd-val tree) del (nd-right tree))
                   (mk-red (nd-key tree) (nd-val tree) del (nd-right tree))))
      :else (let [del (tree-del cmp (nd-right tree) k)]
              (if (black? (nd-right tree))
                (balance-right-del (nd-key tree) (nd-val tree) (nd-left tree) del)
                (mk-red (nd-key tree) (nd-val tree) (nd-left tree) del))))))

;; in-order walk: conj (proj node) for each node, ascending.
(defn- tree-collect [t proj acc]
  (if (nil? t)
    acc
    (tree-collect (nd-right t) proj
                  (conj (tree-collect (nd-left t) proj acc) (proj t)))))

;; The seq is NOT built here. jolt.host/sorted-seq walks the tree with an explicit
;; stack (Clojure's PersistentTreeMap$Seq) in the host, a chunk of up to 32 nodes
;; at a time: O(log n) to the first element, and a full walk cheaper than running
;; the tree into a vector was. mode 0 walks map entries, 1 keys, 2 vals.

(defn- make-sorted [tag tree cnt cmp cmp-fn ops]
  (-> (jolt.host/tagged-table tag)
      (jolt.host/ref-put! :tree tree)
      (jolt.host/ref-put! :cnt cnt)
      (jolt.host/ref-put! :cmp cmp)
      (jolt.host/ref-put! :cmp-fn cmp-fn)
      (jolt.host/ref-put! :ops ops)))

;; entries as a vector (ascending).
(defn- sc-entries [sc proj]
  (tree-collect (sfield sc :tree) proj []))

;; The leftmost and rightmost nodes of the tree, found by following one spine.
;; `first` and `peek` used to run the whole tree into a vector and take an end of
;; it, which is O(n) for a question the shape answers in O(log n) — 190ms against
;; the reference's 0.4us over 200k entries. Clojure reaches these through
;; PersistentTreeMap.min()/max(), which are exactly these two walks.
(defn- nd-leftmost [n] (let [l (nd-left n)] (if l (recur l) n)))
(defn- nd-rightmost [n] (let [r (nd-right n)] (if r (recur r) n)))

;; --- sorted-map ops ---------------------------------------------------------
;; a real map-entry (map-entry? true), so key/val/seq destructuring work like a
;; regular map's entries.
(defn- map-entry [t] (jolt.host/map-entry (nd-key t) (nd-val t)))

(defn- sm-get [sm k not-found]
  (let [n (tree-lookup (sfield sm :tree) (the-cmp sm) k)]
    (if (nil? n) not-found (nd-val n))))

;; Associative.entryAt — the entry carries the node's own key, which is what
;; clojure.core/find hands back (its metadata is read).
(defn- sm-entry-at [sm k]
  (let [n (tree-lookup (sfield sm :tree) (the-cmp sm) k)]
    (when-not (nil? n) (map-entry n))))

(defn- sm-assoc-1 [sm k v]
  (let [cmp (the-cmp sm) tree (sfield sm :tree)
        found (volatile! nil)
        t (tree-ins cmp tree k v found)]
    (if (nil? t)
      ;; the key is present: keep its first-inserted key, replace the value. The
      ;; map is kept only for the SAME value (PersistentTreeMap.assoc's ==): a value
      ;; = to the old one replaces it, so identity-keyed caches see the new one
      (if (identical? v (nd-val @found))
        sm
        (make-sorted :jolt/sorted-map (tree-replace cmp tree k v) (sfield sm :cnt) (sfield sm :cmp) (sfield sm :cmp-fn) (sfield sm :ops)))
      (make-sorted :jolt/sorted-map (blacken t) (inc (sfield sm :cnt)) (sfield sm :cmp) (sfield sm :cmp-fn) (sfield sm :ops)))))

(defn- sm-assoc-many [sm kvs]
  (let [n (count kvs)]
    (when (odd? n)
      (throw (IllegalArgumentException. (str "No value supplied for key: " (nth kvs (dec n))))))
    (loop [m sm i 0]
      (if (< i n)
        (recur (sm-assoc-1 m (nth kvs i) (nth kvs (inc i))) (+ i 2))
        m))))

(defn- sm-dissoc-1 [sm k]
  (let [cmp (the-cmp sm) tree (sfield sm :tree)]
    (if (nil? (tree-lookup tree cmp k))
      sm
      (let [t (tree-del cmp tree k)]
        (make-sorted :jolt/sorted-map (when t (blacken t)) (dec (sfield sm :cnt)) (sfield sm :cmp) (sfield sm :cmp-fn) (sfield sm :ops))))))

(defn- sm-dissoc-many [sm ks] (reduce sm-dissoc-1 sm ks))

;; conj on a map: a [k v] pair (2-vector / map-entry) or a map to merge;
;; nil is a no-op, as in Clojure.
(defn- sm-conj-1 [sm x]
  (cond
    (nil? x) sm
    (map? x) (reduce (fn [m e] (sm-assoc-1 m (first e) (second e))) sm (seq x))
    (and (vector? x) (= 2 (count x))) (sm-assoc-1 sm (nth x 0) (nth x 1))
    ;; any other java.util.Map$Entry (a TreeMap's), as APersistentMap.cons takes
    (map-entry? x) (sm-assoc-1 sm (key x) (val x))
    :else (throw (ex-info "conj on a sorted-map requires a [key value] pair or a map" {}))))

(defn- sm-conj-many [sm xs] (reduce sm-conj-1 sm xs))

;; --- sorted-set ops (elements stored as keys, nil value) --------------------
(defn- ss-get [ss x not-found]
  (let [n (tree-lookup (sfield ss :tree) (the-cmp ss) x)]
    (if (nil? n) not-found (nd-key n))))

;; One walk. PersistentTreeSet.cons walks twice (contains, then add), so a
;; counting comparator sees fewer calls here than on the JVM; the set is not
;; pinned in the corpus for that reason, the map is.
(defn- ss-conj-1 [ss x]
  (let [cmp (the-cmp ss) tree (sfield ss :tree)
        t (tree-ins cmp tree x nil (volatile! nil))]
    (if (nil? t)
      ss
      (make-sorted :jolt/sorted-set (blacken t) (inc (sfield ss :cnt)) (sfield ss :cmp) (sfield ss :cmp-fn) (sfield ss :ops)))))

(defn- ss-conj-many [ss xs] (reduce ss-conj-1 ss xs))

(defn- ss-disj-1 [ss x]
  (let [cmp (the-cmp ss) tree (sfield ss :tree)]
    (if (nil? (tree-lookup tree cmp x))
      ss
      (let [t (tree-del cmp tree x)]
        (make-sorted :jolt/sorted-set (when t (blacken t)) (dec (sfield ss :cnt)) (sfield ss :cmp) (sfield ss :cmp-fn) (sfield ss :ops))))))

(defn- ss-disj-many [ss xs] (reduce ss-disj-1 ss xs))

;; --- the ops tables the host dispatches through ------------------------

(def ^:private sm-ops
  {:count    (fn [sm] (sfield sm :cnt))
   :entries  (fn [sm] (sc-entries sm map-entry))
   :seq      (fn [sm] (jolt.host/sorted-seq (sfield sm :tree) true 0))
   :rseq     (fn [sm] (jolt.host/sorted-seq (sfield sm :tree) false 0))
   :first    (fn [sm] (when-let [t (sfield sm :tree)] (map-entry (nd-leftmost t))))
   :get      sm-get
   :entry-at sm-entry-at
   :contains (fn [sm k] (not (nil? (tree-lookup (sfield sm :tree) (the-cmp sm) k))))
   :assoc    sm-assoc-many
   :dissoc   sm-dissoc-many
   :conj     sm-conj-many
   :empty    (fn [sm] (make-sorted :jolt/sorted-map nil 0 (sfield sm :cmp) (sfield sm :cmp-fn) (sfield sm :ops)))})

(def ^:private ss-ops
  {:count    (fn [ss] (sfield ss :cnt))
   :entries  (fn [ss] (sc-entries ss nd-key))
   :seq      (fn [ss] (jolt.host/sorted-seq (sfield ss :tree) true 1))
   :rseq     (fn [ss] (jolt.host/sorted-seq (sfield ss :tree) false 1))
   :first    (fn [ss] (when-let [t (sfield ss :tree)] (nd-key (nd-leftmost t))))
   :get      ss-get
   :contains (fn [ss x] (not (nil? (tree-lookup (sfield ss :tree) (the-cmp ss) x))))
   :conj     ss-conj-many
   :disj     ss-disj-many
   :empty    (fn [ss] (make-sorted :jolt/sorted-set nil 0 (sfield ss :cmp) (sfield ss :cmp-fn) (sfield ss :ops)))})

;; --- constructors + predicates -----------------------------------------------

(defn sorted-map [& kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map nil 0 nil nil sm-ops) (vec kvs)))

(defn sorted-map-by [comparator & kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map nil 0 (fn->cmp comparator) comparator sm-ops) (vec kvs)))

(defn sorted-set [& xs]
  (ss-conj-many (make-sorted :jolt/sorted-set nil 0 nil nil ss-ops) (vec xs)))

(defn sorted-set-by [comparator & xs]
  (ss-conj-many (make-sorted :jolt/sorted-set nil 0 (fn->cmp comparator) comparator ss-ops) (vec xs)))

(defn sorted-map? [x] (= :jolt/sorted-map (sfield x :jolt/type)))
(defn sorted-set? [x] (= :jolt/sorted-set (sfield x :jolt/type)))
;; A type declaring clojure.lang.Sorted is sorted on the JVM (sorted? is an
;; instance? test there), and subseq / rsubseq reach it through the interface.
;; instance? is a macro defined later (30-macros); instance-check is its runtime fn.
(defn sorted? [x] (or (sorted-map? x) (sorted-set? x) (instance-check 'clojure.lang.Sorted x)))

;; --- subseq / rsubseq ---------------------------------------------------------
;; test is one of < <= > >= applied Clojure-style to the comparator result:
;; keep entries whose (cmp entry-key k) satisfies (test _ 0). Returns a seq or
;; nil, like Clojure.
;;
;; clojure.core's own definitions, over Sorted.seqFrom: jolt.host/sorted-seq-from
;; seeks the bound in O(log n) and hands back the same lazy stack walk seq uses,
;; so walking on costs amortized O(1) per entry in either direction.

(defn- sc-mode [sc] (if (sorted-map? sc) 0 1))

;; jolt's own sorted colls walk their tree; any other Sorted is asked through the
;; interface's comparator, entryKey, seqFrom and seq, as clojure.core does.
(defn- native-sorted? [sc] (or (sorted-map? sc) (sorted-set? sc)))

;; clojure.core/mk-bound-fn
(defn- mk-bound-fn [sc test k]
  (if (native-sorted? sc)
    (let [cmp (the-cmp sc) map? (sorted-map? sc)]
      (fn [e] (test (cmp (if map? (key e) e) k) 0)))
    (let [cmp (.comparator sc)]
      (fn [e] (test (.compare cmp (.entryKey sc e) k) 0)))))

(defn- seq-from [sc k asc?]
  (if (native-sorted? sc)
    (jolt.host/sorted-seq-from (sfield sc :tree) asc? (sc-mode sc) (the-cmp sc) k)
    (.seqFrom sc k asc?)))

(defn- sc-seq [sc asc?]
  (if (native-sorted? sc)
    (jolt.host/sorted-seq (sfield sc :tree) asc? (sc-mode sc))
    (.seq sc asc?)))

(defn- assert-sorted [sc]
  (when-not (sorted? sc)
    (throw (ClassCastException. (str "subseq/rsubseq require a sorted collection: " (class sc))))))

(defn subseq
  ([sc test k]
   (assert-sorted sc)
   (let [include (mk-bound-fn sc test k)]
     (if (or (identical? test >) (identical? test >=))
       (when-let [s (seq-from sc k true)]
         (if (include (first s)) s (next s)))
       (take-while include (sc-seq sc true)))))
  ([sc start-test start-k end-test end-k]
   (assert-sorted sc)
   (when-let [s (seq-from sc start-k true)]
     (take-while (mk-bound-fn sc end-test end-k)
                 (if ((mk-bound-fn sc start-test start-k) (first s)) s (next s))))))

(defn rsubseq
  ([sc test k]
   (assert-sorted sc)
   (let [include (mk-bound-fn sc test k)]
     (if (or (identical? test <) (identical? test <=))
       (when-let [s (seq-from sc k false)]
         (if (include (first s)) s (next s)))
       (take-while include (sc-seq sc false)))))
  ([sc start-test start-k end-test end-k]
   (assert-sorted sc)
   (when-let [s (seq-from sc end-k false)]
     (take-while (mk-bound-fn sc start-test start-k)
                 (if ((mk-bound-fn sc end-test end-k) (first s)) s (next s))))))
