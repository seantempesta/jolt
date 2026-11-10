(ns app.core)

;; scalar-replace drops a non-escaping (->P ..) let binding and moves its field
;; args to each (:x p) read. Moving them into a fn body left the literal's
;; registered captures naming `p`, a binding that no longer exists, so the
;; image maker the site calls read an unbound variable (the ray tracer's
;; "variable vertical is not bound"). Moving them under a binder that rebinds
;; one of their locals made them read the wrong value. Each fn is called through
;; apply so it is not spliced into -main, where the inliner's renaming hides it.

(defrecord P [x y])

;; captured by a closure
(defn closure-capture [a b]
  (let [p (->P (* a 2) (* b 3))]
    (reduce (fn [acc _] (+ acc (:x p) (:y p))) 0 (range 2))))

;; a let rebinding a local the ctor args read
(defn let-shadow [a]
  (let [p (->P a 2)]
    (let [a 10] (+ (:x p) (:y p)))))

;; a loop binding
(defn loop-shadow [a]
  (let [p (->P a 2)]
    (loop [a 0 acc 0]
      (if (< a 2) (recur (inc a) (+ acc (:x p) (:y p))) acc))))

;; a fn param
(defn param-shadow [a]
  (let [p (->P a 2)]
    (mapv (fn [a] (+ (:x p) (:y p))) [100 200])))

;; a catch binding
(defn catch-shadow [a]
  (let [p (->P a 2)]
    (try (throw (ex-info "x" {}))
         (catch Exception a (+ (:x p) (:y p))))))


;; an allocating field value read twice, or once per loop iteration: moved to
;; each read it made a new object per read where the binding held one
(defn alloc-twice [a]
  (let [m {:x [a] :y 2} p (->P [a] 2)]
    [(identical? (:x m) (:x m)) (identical? (:x p) (:x p)) (:y m) (:y p)]))

(defn alloc-loop [a]
  (let [m {:x [a] :y 2}]
    (loop [i 0 prev (:x m)]
      (if (< i 2) (recur (inc i) (:x m)) (identical? prev (:x m))))))

(defn -main [& _]
  (println (apply closure-capture [1 2]))
  (println (apply let-shadow [1]))
  (println (apply loop-shadow [1]))
  (println (apply param-shadow [1]))
  (println (apply catch-shadow [1]))
  (println (apply alloc-twice [1]))
  (println (apply alloc-loop [1])))
