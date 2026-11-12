;; A var's watch and its root are ordered as the JVM orders them: a context that adds a
;; watch to a var and then reads it, against a context that sets the var's root (a def,
;; an intern), never misses both, the watch not called and the read seeing the root from
;; before. On the JVM, Var.bindRoot is synchronized, sets the volatile root and then
;; notifies the watches, so either the add comes first (the watch is called) or the root
;; is set first (the read sees it).
;;
;; Each case runs N rounds on one var, the two contexts stepped together round by round
;; so that their accesses meet: the watcher takes its watch off before the round starts
;; and adds it again inside it, so each round is a first watch against a root set. Round
;; i sets the root to i+1, and the watch marks the round of the root it was called with.
;; Two OS threads, then two fibers on two carriers (two pools of one carrier each). Each
;; case prints its name, then ok or FAIL with the count of rounds where both missed.
(ns var-watch-order
  (:require [jolt.fibers :as fib]))

(def n 200000)

(def v 0)
(def w 0)

(defn step-together
  "Run (a i) in one context and (b i) in another for i below n, each round started only
   once the other has finished the round before; (pre i) runs in b's context before
   round i, while a waits. RUN starts a context and returns a function that joins it."
  [run pre a b]
  (let [ca (atom -1) cb (atom -1)
        ja (run (fn [] (dotimes [i n] (reset! ca i) (while (< @cb i)) (a i))))
        jb (run (fn [] (dotimes [i n] (while (< @ca i)) (pre i) (reset! cb i) (b i))))]
    (ja) (jb)))

(defn on-threads [f]
  (let [t (Thread. f)] (.start t) #(.join t)))

(defn on-carrier
  "Each context its own pool of one carrier, so the two run on two carriers."
  [f]
  (let [p (fib/pool {:size 1})
        fb (fib/spawn f {:pool p})]
    #(do (fib/join fb) (fib/shutdown! p))))

(defn case! [label run the-var set-root!]
  (let [fired (boolean-array n)
        read  (long-array n)]
    (set-root! -1)
    (step-together
      run
      (fn [_] (remove-watch the-var ::k))
      (fn [i] (set-root! i))
      (fn [i]
        (add-watch the-var ::k (fn [_ _ _ new] (when (<= 1 new n) (aset fired (dec new) true))))
        (aset read i (long @the-var))))
    (remove-watch the-var ::k)
    (let [missed (count (filter #(and (not (aget fired %)) (<= (aget read %) %)) (range n)))]
      (println (str "var-watch-order " label ": "
                    (if (zero? missed) "ok" (str "FAIL " missed " of " n " rounds missed both"))))
      (zero? missed))))

(defn cases [ctx run]
  [(case! (str "def, " ctx) run #'v (fn [i] (def v (inc i))))
   (case! (str "intern, " ctx) run #'w (fn [i] (intern 'var-watch-order 'w (inc i))))
   (case! (str "alter-var-root, " ctx) run #'v (fn [i] (alter-var-root #'v (constantly (inc i)))))])

(let [oks (concat (cases "two threads" on-threads)
                  (cases "two fibers on two carriers" on-carrier))]
  (if (every? true? oks)
    (do (println "var-watch-order: passed") (System/exit 0))
    (do (println "var-watch-order: failed") (System/exit 1))))
