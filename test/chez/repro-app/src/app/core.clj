;; jolt build twice, cold and warm caches: every unit's text must be identical
;; (host/chez/repro-build-smoke.sh). Shapes whose emission could depend on hash
;; or allocation order: constant sets and maps, case dispatch tables, records,
;; protocols, multimethods, reify, gensyms in syntax-quote and destructuring.
(ns app.core
  (:require [app.model :as m]
            [clojure.string :as str]))

(def kws #{:alpha :beta :gamma :delta :epsilon :zeta :eta :theta :iota :kappa})
(def table {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 'sym "s" "str" \c})
(def nested '{:x #{1 2 3 [4 5 {:y #{:z}}]} :w (a b #{c d})})

(defn kind [x]
  (case x
    (:alpha :beta) :early
    (:gamma :delta :epsilon) :middle
    "str" :string
    42 :answer
    [1 2] :pair
    :other))

(defn shapes [n]
  (for [i (range n)
        :let [s (if (even? i) (m/->Square i) (m/->Circle i))]
        :when (pos? (m/area s))]
    {:label (m/label s) :area (m/area s)}))

(defn render [{:keys [label area] :as row} & {:keys [sep] :or {sep ": "}}]
  (str label sep area))

(def counter
  (reify clojure.lang.IDeref
    (deref [_] (count kws))))

(defn -main [& args]
  (println (str/join "," (map render (shapes 4))))
  (println (map kind [:beta :delta "str" 42 [1 2] :nope]))
  (println (m/describe {:kind :dot :n 3}) (m/describe {:kind :x}))
  (println (m/with-twice [y 21] y) @counter (count table) (count nested))
  (letfn [(ev? [n] (if (zero? n) true (od? (dec n))))
          (od? [n] (if (zero? n) false (ev? (dec n))))]
    (println (ev? 10) (sort (map name kws)))))
