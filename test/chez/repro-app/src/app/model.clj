(ns app.model)

(defprotocol Shape
  (area [s])
  (label [s]))

(defrecord Square [w]
  Shape
  (area [_] (* w w))
  (label [_] "square"))

(deftype Circle [r]
  Shape
  (area [_] (* 3 r r))
  (label [_] "circle"))

(defmulti describe :kind)
(defmethod describe :dot [m] (str "dot " (:n m)))
(defmethod describe :default [m] (str "thing " (pr-str m)))

(defmacro with-twice [[sym v] & body]
  `(let [x# ~v ~sym (+ x# x#)] ~@body))
