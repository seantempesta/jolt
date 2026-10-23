(ns jolt.passes.fold
  "Constant folding (always-on IR pass) plus the shared const-shape predicate.
  Bottom-up numeric folding + dead-branch and unread-binding removal, total over node :ops (unknown
  ops pass through with folded children). Portable Clojure: kernel-tier fns +
  seed primitives only — it loads with the compiler namespaces, before the later
  core tiers."
  (:require [jolt.ir :refer [map-ir-children reduce-ir-children]]
            [jolt.op-registry :as op-registry]))

;; Folding computes with THE ACTUAL jolt fns, so a folded result matches what
;; the unfolded code would produce at runtime by construction. Conservative:
;; numbers only, the op table only names pure numeric fns, and any throw
;; during folding (e.g. (mod x 0)) leaves the node alone for runtime.
;; op-name -> the ACTUAL jolt fn used to fold it. SEED fns only: this ns loads
;; with the compiler, BEFORE the later core tiers — a name from 20-coll
;; (min/max/abs) wouldn't resolve yet. The __bit-* seams back bit-and/or/xor
;; because the public bit fns are 20-coll variadic shells that don't exist yet;
;; folding stays 2-arg (a 3+-arg constant call throws arity in the fold and is
;; left for runtime).
(def ^:private foldable-fns
  {"+" + "-" - "*" * "/" /
   "<" < ">" > "<=" <= ">=" >= "=" =
   "inc" inc "dec" dec
   "mod" mod "rem" rem "quot" quot
   "bit-and" __bit-and "bit-or" __bit-or "bit-xor" __bit-xor})
;; the registry's :foldable? set is the authoritative membership; select-keys
;; keeps only the impls it names, so the two can't drift.
(def ^:private foldable (select-keys foldable-fns op-registry/foldable-ops))

(defn- const? [n] (= :const (get n :op)))
(defn- const-num? [n] (and (const? n) (number? (get n :val))))

(defn- fold-fn [fnode]
  (let [op (get fnode :op)]
    (when (or (and (= op :var) (= "clojure.core" (get fnode :ns)))
              (= op :host))
      (get foldable (get fnode :name)))))

;; A let binding nothing after it reads, whose init only reads (a local, a
;; constant, a get or keyword lookup of them, a deftype field's slot), is dropped:
;; its value was never used and the read has no effect and cannot throw.
;; deftype and defrecord methods bind every field of their type, as Clojure's do,
;; and this keeps only the ones the method's analyzed (macroexpanded) body reads:
;; a protocol call on a 21-field record read all 21 (~6 ticks each in the runtime
;; spine) to use one. A field named like a var, or reached only through a macro,
;; is a :local of the expanded body, so it is read and kept. __deftype-slot is
;; only emitted by deftype on the method's own instance, where it cannot throw.
(declare kw-callee? get-callee?)

(defn- read-only? [n]
  (let [op (get n :op)]
    (or (= op :local) (= op :const)
        (and (= op :invoke)
             (let [f (get n :fn) args (get n :args) k (count args)]
               (and (every? (fn [a] (let [o (get a :op)] (or (= o :local) (= o :const)))) args)
                    (or (and (kw-callee? f) (or (= k 1) (= k 2)))
                        (and (get-callee? f) (or (= k 2) (= k 3)))
                        (and (= :var (get f :op)) (= "clojure.core" (get f :ns))
                             (= "__deftype-slot" (get f :name)) (= k 3)))))))))

;; the names of the locals node reads, added to acc
(defn- local-reads [node acc]
  (if (= :local (get node :op))
    (conj acc (get node :name))
    (reduce-ir-children (fn [a c] (local-reads c a)) acc node)))

(defn- drop-unread-reads [node]
  (let [binds (get node :bindings)]
    (if (or (get node :letrec)
            (not (some (fn [b] (read-only? (nth b 1))) binds)))
      node
      ;; from the last binding back: one is read if the body or a kept later
      ;; init reads its name (a later rebinding of the name keeps it too)
      (loop [i (dec (count binds)) used (local-reads (get node :body) #{}) kept ()]
        (if (< i 0)
          (cond (= (count kept) (count binds)) node
                (empty? kept) (get node :body)
                :else (assoc node :bindings (vec kept)))
          (let [b (nth binds i)]
            (if (and (read-only? (nth b 1)) (not (contains? used (nth b 0))))
              (recur (dec i) used kept)
              (recur (dec i) (local-reads (nth b 1) used) (cons b kept)))))))))

(defn const-fold
  "Bottom-up constant folding: a call of a foldable numeric fn whose args are
  all constant numbers becomes a constant; an if with a constant test becomes
  the taken branch; a let binding nothing reads, whose init only reads, is
  dropped."
  [node]
  (let [op (get node :op)]
    (cond
      (= op :invoke)
      ;; fold children first, then this call if the fn is foldable over consts
      (let [n (map-ir-children const-fold node)
            ff (fold-fn (get n :fn))
            args (get n :args)
            folded (when (and ff (pos? (count args)) (every? const-num? args))
                     (try
                       {:op :const :val (apply ff (mapv (fn [a] (get a :val)) args))}
                       ;; :default (not Exception) — match the rest of jolt-core and
                       ;; also catch a raw host condition from a folding primitive.
                       (catch :default e nil)))]
        (or folded n))

      (= op :if)
      (let [t (const-fold (get node :test))]
        (if (const? t)
          ;; jolt truthiness = Clojure's: nil/false take else
          (if (or (nil? (get t :val)) (= false (get t :val)))
            (const-fold (get node :else))
            (const-fold (get node :then)))
          (assoc node
                 :test t
                 :then (const-fold (get node :then))
                 :else (const-fold (get node :else)))))

      (= op :let) (drop-unread-reads (map-ir-children const-fold node))

      ;; every other op: fold each child (let/loop bindings are [name init]
      ;; pairs, handled by the combinator)
      :else (map-ir-children const-fold node))))

;; A const node whose value is a scalar literal (kw/str/num/bool). Shared by the
;; scalar-replace pass (jolt.passes.inline) and the collection-type inference
;; (jolt.passes.types), which both reason about const-keyed maps.
(defn scalar-const? [n]
  (and (= :const (get n :op))
       (let [v (get n :val)] (or (keyword? v) (string? v) (number? v) (boolean? v)))))

;; The two callee shapes a constant-keyword map lookup takes: a keyword in fn
;; position — (:k m) — or clojure.core/get with a const key — (get m :k). The
;; inliner (scalar replacement) and the type inferencer both recognize these;
;; sharing the head predicates keeps the two from drifting. Each caller still
;; imposes its own arity, subject, and key-type constraints.
(defn kw-callee?
  "True if fnode is a constant keyword used as a function head — the (:k m) form."
  [fnode]
  (and (= :const (get fnode :op)) (keyword? (get fnode :val))))

(defn get-callee?
  "True if fnode is the clojure.core/get (or host get) callee — the (get m k) form."
  [fnode]
  (or (and (= :var (get fnode :op)) (= "clojure.core" (get fnode :ns)) (= "get" (get fnode :name)))
      (and (= :host (get fnode :op)) (= "get" (get fnode :name)))))
