(ns profile-fixture)

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn sum-to [n]
  (loop [i 0 acc 0]
    (if (< i n)
      (recur (inc i) (+ acc i))
      acc)))

(defprotocol Shape (area [s]))

(defrecord Square [w]
  Shape
  (area [_] (* w w)))

(defmulti describe :kind)
(defmethod describe :dot [m]
  (str "dot " (:n m)))

(defn eval-loop []
  (eval (list 'loop '[i 0] (list 'if '(< i 1000) '(recur (inc i)) 'i))))
