(ns fa.lib)
;; Named like clojure.core's lazy step (map-seq) and like fa.core's own fns: a frame
;; is named for the var whose procedure it is, never for another var sharing the name.
(defn map-seq [x] x)
