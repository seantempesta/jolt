;; An atom's watch and its value are ordered as the JVM orders them: a thread that adds
;; a watch and then derefs the atom, against a thread that changes the atom, never
;; misses both, the watch not called and the deref reading the value from before. On
;; the JVM, ARef's watches and the AtomicReference are volatile, so the two threads'
;; accesses fall in one order, and either the change comes after the add (the watch is
;; called) or before the deref (the deref reads it). Without a store-load fence after
;; each side's store, AArch64 lets each thread's load pass its own store, and both
;; miss: a wait that adds a watch, then checks the value, then parks waits for good
;; (Datahike's writer waiting for its connection did, one connect in some thousands).
;;
;; Each case runs N rounds on fresh atoms, the two threads stepped together round by
;; round so that their accesses meet, and prints its name, then ok or FAIL with the
;; count of rounds where both missed.
(def n 200000)

(defn step-together
  "Run (a i) on one thread and (b i) on another for i below n, each round started
   only once the other thread has reached it."
  [a b]
  (let [ca (atom -1) cb (atom -1)
        ta (Thread. (fn [] (dotimes [i n] (reset! ca i) (while (< @cb i)) (a i))))
        tb (Thread. (fn [] (dotimes [i n] (reset! cb i) (while (< @ca i)) (b i))))]
    (.start ta) (.start tb) (.join ta) (.join tb)))

(defn case! [label change!]
  (let [atoms (vec (repeatedly n #(atom 0)))
        fired (boolean-array n)
        read  (long-array n)]
    (step-together
      (fn [i] (let [a (nth atoms i)]
                (add-watch a ::k (fn [_ _ _ _] (aset fired i true)))
                (aset read i (long @a))))
      (fn [i] (change! (nth atoms i))))
    (let [missed (count (filter #(and (not (aget fired %)) (zero? (aget read %))) (range n)))]
      (println (str "atom-watch-order " label ": "
                    (if (zero? missed) "ok" (str "FAIL " missed " of " n " rounds missed both"))))
      (zero? missed))))

(let [oks [(case! "reset!" #(reset! % 1))
           (case! "swap!" #(swap! % inc))
           (case! "compare-and-set!" #(compare-and-set! % 0 1))
           (case! "swap-vals!" #(swap-vals! % inc))
           (case! "reset-vals!" #(reset-vals! % 1))]]
  (if (every? true? oks)
    (do (println "atom-watch-order: passed") (System/exit 0))
    (do (println "atom-watch-order: failed") (System/exit 1))))
