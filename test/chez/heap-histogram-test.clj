;; An OutOfMemoryError says what holds the heap: its ex-data is {:jolt/heap {:live :static :dynamic
;; :ceiling :top [{:type :count :bytes :gens} ...]}}, the top types by collectable bytes, from one more
;; full collection with Chez's per-type counts on (rt.ss heap-histogram->jolt), and jolt.host/heap-histogram
;; answers the same shape on demand. Run by `make heaphistogram` under JOLT_MAX_HEAP=256m.
(defn check [what ok?] (when-not ok? (println "FAIL heap-histogram:" what) (System/exit 1)))

;; on demand: a held 40MB of byte arrays is the top type, and the totals add up
(def held (vec (repeatedly 40 #(byte-array 1000000))))
(def h (jolt.host/heap-histogram 20))
(println "on demand top 3:" (pr-str (take 3 (:top h))))
(check "the shape" (every? #(contains? h %) [:live :static :dynamic :ceiling :top]))
(check "at most 20 types" (<= (count (:top h)) 20))
(check "bytes descend" (apply >= (map :bytes (:top h))))
(check "the held byte arrays are the top type" (>= (:bytes (first (:top h))) 40000000))
(check "dynamic is under live" (<= (:dynamic h) (:live h)))

;; at the ceiling: the error carries the same, naming the type that fills the heap
(def e (try (loop [acc ()] (recur (conj acc (byte-array 1000000))))
            (catch OutOfMemoryError e e)))
(def hp (:jolt/heap (ex-data e)))
(println "OOM message:" (ex-message e))
(println "OOM :jolt/heap" (pr-str (-> hp (update :top #(vec (take 3 %))))))
(check "an OutOfMemoryError was caught" (instance? OutOfMemoryError e))
(check "its message is kept" (.contains (str (ex-message e)) "the heap ceiling of"))
(check "it carries the histogram" (map? hp))
(check "the ceiling is the one asked for" (= 268435456 (:ceiling hp)))
(check "the top type holds most of the heap" (> (:bytes (first (:top hp))) (* 0.5 (:ceiling hp))))
(check "and it is the same type as on demand" (= (:type (first (:top h))) (:type (first (:top hp)))))
(println "HEAP-HISTOGRAM-TEST OK")
