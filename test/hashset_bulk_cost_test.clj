;; The bulk Collection methods of java.util.HashSet and ArrayList answer as the JDK's
;; do, at the JDK's cost: one membership test of the argument (its own contains) an
;; element, so the work is linear in the two sizes when the argument is hashed.
;;
;; HashSet.retainAll turned its argument into a list and scanned it with = for every
;; element of the set: n x m. A diff of two Datahike trees that kept the addresses
;; two HashSets share spent 18.9 M of its 22.2 M ticks there (two sets of a few
;; thousand). Each row below runs its op at n and 10n and asks that the engine ticks
;; grow about 10x (a linear op), not 100x (a scan an element); ticks are the work
;; the engine did, so the gate does not read the machine's load.

(ns hashset-bulk-cost-test)

(def failures (atom []))
(defn- check [what ok? got] (when-not ok? (swap! failures conj (str what ": got " (pr-str got)))))

;; a deterministic stream of small ints, so the sets overlap and a run repeats
(defn- ints [seed k bound]
  (->> (iterate #(mod (+ (* % 1103515245) 12345) 2147483648) seed) rest (map #(mod (quot % 65536) bound)) (take k)))

(defn- hs [xs] (java.util.HashSet. ^java.util.Collection (vec xs)))
(defn- al [xs] (java.util.ArrayList. ^java.util.Collection (vec xs)))

;; each argument shape the JDK accepts: a hashed set of either kind, a list of either
;; kind (with duplicates), and the empty ones
(def arg-shapes
  {"HashSet" hs "Clojure set" set "vector" vec "list" #(apply list %) "ArrayList" al
   "empty HashSet" (fn [_] (java.util.HashSet.)) "empty set" (fn [_] #{}) "empty vector" (fn [_] [])})

;; what each op answers on the JDK: the receiver after it, and whether it changed
(defn- expect [op recv arg]
  (let [r (set recv) a (set arg)]
    (case op
      :retainAll [(clojure.set/intersection r a) (not= r (clojure.set/intersection r a))]
      :removeAll [(clojure.set/difference r a) (not= r (clojure.set/difference r a))]
      :addAll [(clojure.set/union r a) (not= r (clojure.set/union r a))]
      :containsAll [r (every? r arg)])))

(defn- run-op [op ^java.util.Collection c arg]
  (case op
    :retainAll (.retainAll c arg)
    :removeAll (.removeAll c arg)
    :addAll (.addAll c arg)
    :containsAll (.containsAll c arg)))

(doseq [seed [1 7 42 1000] [aname mk] arg-shapes op [:retainAll :removeAll :addAll :containsAll]
        [rname mk-recv] {"HashSet" hs "ArrayList" al}]
  (let [recv-xs (ints seed 40 60)
        arg-xs (ints (* 3 seed) 30 60)
        recv (mk-recv recv-xs)
        arg (mk arg-xs)
        arg-seq (vec (seq arg))
        [want-elems want-changed] (expect op recv-xs arg-seq)
        got-changed (run-op op recv arg)
        what (str rname " " (name op) " of a " aname " (seed " seed ")")]
    (check (str what ": its elements") (= want-elems (set recv)) (sort recv))
    (check (str what ": the answer") (= want-changed got-changed) got-changed)
    ;; an ArrayList keeps its order and its duplicates: removeAll and retainAll filter it
    (when (and (= rname "ArrayList") (#{:retainAll :removeAll} op))
      (let [keep? (if (= op :retainAll) (set arg-seq) (complement (set arg-seq)))]
        (check (str what ": the order and duplicates kept") (= (filter keep? recv-xs) (vec recv)) (vec recv))))
    (check (str what ": the argument untouched") (= arg-seq (vec (seq arg))) (vec (seq arg)))))

;; the cost: the least of a few runs' ticks, each on a fresh receiver made outside the measure
(defn- ticks [mk-recv arg op]
  (apply min (for [_ (range 3)] (let [c (mk-recv)] (jolt.host/ticks-of #(run-op op c arg))))))

(def n 1000)
(def max-ratio 20) ; n vs 10n: linear about 10, a scan an element about 100

(defn- judge [what f]
  (let [t1 (f n) t10 (f (* 10 n)) ratio (/ (double t10) (max 1 t1))]
    (println (format "  %-48s %10d %12d ticks  x%.1f" what t1 t10 ratio))
    (check (str what " grows linearly (n " n " vs " (* 10 n) ": " t1 " vs " t10 " ticks)") (< ratio max-ratio) ratio)))

(println (str "hashset-bulk-cost: ticks at n=" n " and " (* 10 n)))
;; half of each receiver's elements are in the argument, so every op does its whole work
(doseq [[aname mk] {"HashSet" hs "Clojure set" set}
        op [:retainAll :removeAll :containsAll :addAll]]
  (judge (str "HashSet " (name op) " of a " aname)
         (fn [k] (let [arg (mk (range (quot k 2) (+ k (quot k 2))))]
                   (ticks #(hs (range k)) arg op)))))
(doseq [[aname mk] {"HashSet" hs "Clojure set" set} op [:retainAll :removeAll]]
  (judge (str "ArrayList " (name op) " of a " aname)
         (fn [k] (let [arg (mk (range 0 k 2))]
                   (ticks #(al (range k)) arg op)))))
(judge "HashSet equals a HashSet"
       (fn [k] (let [a (hs (range k)) b (hs (reverse (range k)))]
                 (apply min (for [_ (range 3)] (jolt.host/ticks-of #(.equals a b)))))))
(judge "HashSet = a Clojure set"
       (fn [k] (let [a (hs (range k)) b (set (range k))]
                 (apply min (for [_ (range 3)] (jolt.host/ticks-of #(= a b)))))))

(if (seq @failures)
  (do (doseq [f @failures] (println "FAIL" f))
      (println (str "hashset-bulk-cost: " (count @failures) " failed"))
      (System/exit 1))
  (println "hashset-bulk-cost: passed"))
