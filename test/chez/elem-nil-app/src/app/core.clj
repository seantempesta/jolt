(ns app.core)

;; An element read off a typed vector may be nil: (nth v i default) answers the
;; default past the end, and first/second/peek/last/fnext answer nil on a short
;; vector. Inference typed each as the element (:str here), so nil? folded false
;; and a destructured missing slot reached parse-long/count as nil.
(defn split [s] (let [i (.indexOf ^String s "-")]
                  (if (neg? i) [(subs s 0)] [(subs s 0 i) (subs s (inc i))])))

(defn to [s] (let [[_ t] (split s)] (if (nil? t) :none (parse-long t))))

(defn -main [& _]
  (println (to "5-9") (to "50"))
  (println (some? (second (split "50"))) (nil? (first (subvec (split "50") 1)))
           (nil? (peek (pop (split "50")))) (nil? (last (pop (split "50"))))
           (nil? (fnext (split "50"))) (some? (nth (split "50") 3 nil))))
