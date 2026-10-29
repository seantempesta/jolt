;; The collector policy (host/chez/rt.ss jolt-install-gc-policy!): the nursery
;; (the trip threshold) grows while collections take a large share of the time
;; and stays at its 16MB floor for a program that allocates little, and
;; JOLT_GC_TRIP_BYTES pins it. Run by `make gcpolicy` as three processes, one per
;; mode, each printing what its nursery ended at.
;;
;; A fixed 16MB nursery made writ's prover spend 40% of its time collecting:
;; medium-lived data (live across a 16MB window, dead soon after) was copied on
;; every young collection and again through the older generations. The shape here
;; is the same: a working set rebuilt constantly, retained just long enough.
(ns gc-policy-test)

(def ^:private settled (atom nil))

(defn churn []
  ;; a working set of 200k small maps, rebuilt from scratch each round and kept
  ;; until the next is built: a fixed ~40MB live, several GB allocated in all.
  ;; (Built from the index, not from the previous round's values, so it cannot
  ;; grow: a (str x) of the previous element doubles every round.)
  ;; The policy's growth probe jumps the nursery to 8x for eight collections
  ;; before it settles (rt.ss gc-check-growth!), so the figure at the instant
  ;; the loop ends can be that transient. The settled size is the median of the
  ;; trip read after each of the last 30 rounds.
  (loop [i 0 window [] seen []]
    (if (< i 60)
      (let [w (mapv (fn [j] {:i j :s (str j "-" i)}) (range 200000))]
        (recur (inc i) w (if (>= i 30) (conj seen (jolt.host/gc-trip-bytes)) seen)))
      (do (reset! settled (nth (vec (sort seen)) (quot (count seen) 2)))
          (count window)))))

;; For the ceiling: 600k small maps held (~100MB, promoted through the
;; generations by scheduled collections, which is what copies them) while the
;; churn runs beside them.
(def ^:private held (atom nil))
(defn- hold-and-churn []
  (reset! held (vec (map (fn [i] {:i i :s (str "held-" i)}) (range 600000))))
  (jolt.host/reset-maximum-memory-bytes!)
  (loop [i 0 window []]
    (if (< i 40)
      (recur (inc i) (mapv (fn [j] {:i j :s (str j "-" i)}) (range 200000)))
      (+ (count window) (count @held)))))

;; For the refresh: a full collection the program asks for measures the live set
;; as the policy's own do, so the older generations' allowance afterwards is sized
;; from what is live NOW. Held data grows the allowance past its 64MB minimum;
;; dropped and collected, it shrinks with the live set. It need not land on the
;; minimum: building the held data can cross the old-generation limit, and a
;; full collection the policy runs then may raise its growth factor. Before,
;; System/gc left the policy's figure where its last own full collection put it.
;; The allowance is sized from the collectable heap (the static boot image left
;; out), so with ~100MB held it is about that, and dropped it falls to the 64MB
;; minimum: the gate asks only that it shrank, since a stale figure would not.
(defn- refresh []
  (reset! held (vec (map (fn [i] {:i i :s (str "held-" i)}) (range 600000))))
  (System/gc)
  (let [with-held (jolt.host/gc-old-growth-bytes)]
    (reset! held nil)
    (System/gc)
    (println "growth" with-held (jolt.host/gc-old-growth-bytes))))

;; For nepotism: a walk down one long lazy seq keeps nothing it has passed, so
;; what the heap holds after each young collection stays flat. With Chez's own
;; schedule the cell the walk was on at a collection was promoted, then its
;; tail was realized into it, and the dead promoted cell rooted every cell
;; realized after it: the heap after each collection climbed by most of a
;; nursery until a full collection.
(defn- walk []
  (reduce (fn [a x] (mod (+ a x) 1000000007)) 0
          (take 6000000 (iterate (fn [x] (mod (+ x 3) 1000000007)) 1))))

;; For the startup reading: a built binary's first collection comes within a
;; millisecond of the policy starting and reads as most of that millisecond. The
;; share averaged each collection's own ratio, so that one reading doubled the
;; nursery, and the next doubled it again though collection took 2% of it: a 9ms
;; benchmark ran in a 42MB nursery. Fed the same readings, the policy now keeps
;; the floor (a share of summed times, and nothing resized before five
;; collections, as the JVM's AdaptiveSizePolicyReadyThreshold). The live policy
;; is reset first; the readings are nanoseconds (gc, elapsed).
(defn- startup []
  (jolt.host/scheme-eval-string
   "(begin (set! gc-share-gc 0.0) (set! gc-share-el 0.0) (set! gc-share 0.0) (set! gc-seen 0)
           (set! gc-probe-from #f) (set! gc-growth-hold #f) (gc-win-reset!)
           (sa-gc-trip-bytes! gc-trip-floor)
           (gc-size-nursery! 400000 500000)
           (for-each (lambda (i) (gc-size-nursery! 120000 6000000)) '(1 2 3 4 5 6)))"))

;; For the reserve: with ~100MB held, the free memory a full collection keeps
;; (Chez's heap-reserve-ratio) is what JOLT_MAX_HEAP_FREE_RATIO allows, P/(100-P)
;; of the heap in use: a ceiling far above the heap fitted it at 1x whatever P said.
(defn- reserve []
  (reset! held (vec (map (fn [i] {:i i :s (str "held-" i)}) (range 600000))))
  (System/gc)
  (println "reserve" (jolt.host/scheme-eval-string "(heap-reserve-ratio)")))

(defn -main [mode]
  (case mode
    "refresh" (refresh)
    "reserve" (reserve)
    "churn" (churn)
    "light" (reduce + (range 1000))
    "walk" (walk)
    "startup" (startup)
    "pinned" (churn)
    ;; the heap ceiling bounds the TOTAL heap, as -Xmx does: live data, nursery
    ;; and the free memory kept (the gate reads the high-water mark and the GC log)
    "ceiling" (hold-and-churn))
  (println "trip" (or @settled (jolt.host/gc-trip-bytes)))
  (when (= mode "ceiling")
    ;; one write of the whole line: println of several args writes each piece
    ;; separately, and the GC log (another thread, 2>&1) landed between them.
    (print (str "peak " (jolt.host/maximum-memory-bytes) " max " (.maxMemory (Runtime/getRuntime)) "\n"))))

(apply -main *command-line-args*)
