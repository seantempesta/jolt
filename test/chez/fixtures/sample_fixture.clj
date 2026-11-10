(ns sample-fixture)

(defn spin [n]
  (loop [i 0 acc 0]
    (if (< i n)
      (recur (inc i) (+ acc (bit-and i 7)))
      acc)))

(defn hot [n] (inc (spin (* 3 n))))

(defn cold [n] (inc (spin n)))

;; hot and cold take turns, in rounds much longer than a 1 ms interval, so a
;; stretch of noise in the CPU clock (a CI runner's) lands on both of them
;; rather than on whichever ran through it
(defn driver [n]
  (let [m (quot n 8)]
    (loop [r 0 acc 0]
      (if (< r 8)
        (recur (inc r) (+ acc (hot m) (cold m)))
        acc))))
