;; Analysis allocates the IR, not its lookups: what analyzing a form allocates
;; grows with its nodes at a ceiling per node kind, measured as the slope between
;; two sizes (n and 10n) of one shape, in bytes. Bytes are deterministic where time
;; is not: each size is the least of several runs (a collection or a runtime thread
;; only ever adds), and the slope cancels the fixed per-form cost.
;;
;; An IR node is about 100-130 bytes; on 195 real forms (31,537 nodes) analysis
;; allocated 2,907 B a node, about 25x the IR it built. What it paid for:
;; - every list form read *unchecked-math* and jolt.host/*invoke-rewrite* by
;;   building an "ns/name" string and probing the var table inside a guard
;;   (host-contract.ss hc-unchecked-math?, hc-invoke-rewriter): now the cell, once;
;; - every macro call walked its whole call form to turn the reader's set forms
;;   into sets (hc-expand-1's hc-macro-arg), a list cell per node, so nested macros
;;   were quadratic (100 nested whens 1.69 MB, 200 5.28 MB); now the top-level
;;   form is scanned once (form-holds-set-form?) and a form holding none is not
;;   walked again;
;; - a var reference resolved its symbol three times (resolve-global, then
;;   form-macro? twice) and a macro call its head four: resolve-global answers
;;   :macro, and the macro arm resolves once;
;; - every macro call rebuilt &env from every local in scope: now kept in env as
;;   the locals are added, which a binding pays for instead (the let row).
;; Measured 2026-10-23, before -> after (B a unit): an (inc ...) level 4,536 ->
;; 2,241; a var reference 889 -> 601; a let binding 1,742 -> 2,565; a when inside
;; eight locals 63,076 (at n 50-500, quadratic) -> 4,689. Each ceiling is the
;; after with ~10% room.

(ns analyze-alloc-test
  (:require [jolt.analyzer :as ana]))

(defn- bytes-now [] (jolt.host/scheme-eval-string "(sstats-bytes (statistics))"))

(defn- analyze-bytes
  "The least bytes analyzing form allocated over a few runs."
  [form]
  (let [ctx (jolt.host/ctx-for-ns "analyze-alloc-test")]
    (ana/analyze ctx form)
    (apply min (for [_ (range 5)]
                 (let [b0 (bytes-now)] (ana/analyze ctx form) (- (bytes-now) b0))))))

(defn- slope
  "Bytes per unit of shape f between n and 10n units."
  [f n]
  (/ (double (- (analyze-bytes (f (* 10 n))) (analyze-bytes (f n)))) (* 9 n)))

(defn- nest [n wrap leaf] (reduce (fn [x _] (wrap x)) leaf (range n)))

(def shapes
  "Each shape of n units, and its ceiling in bytes a unit."
  [["(inc …) level: an invoke of a var around a local"
    (fn [n] (list 'fn* '[x] (nest n #(list 'inc %) 'x))) 2500]
   ["var reference, as a vector element"
    (fn [n] (vec (repeat n 'clojure.core/inc))) 660]
   ["let binding"
    (fn [n] (list 'let* (vec (mapcat (fn [i] [(symbol (str "a" i)) i]) (range n))) 'a0)) 2800]
   ["macro call (when) inside eight locals"
    (fn [n] (list 'fn* '[a b c d e f g h] (nest n #(list 'when 'a %) 'b))) 5200]])

(def failures (atom 0))
(doseq [[what f ceiling] shapes]
  (let [s (slope f 50)]
    (println (format "%-48s %8.0f B a unit (ceiling %d)" what s ceiling))
    (when (> s ceiling)
      (swap! failures inc)
      (println "FAIL:" what "allocates" (long s) "B a unit, past" ceiling))))
(if (zero? @failures)
  (println "analyze-alloc: OK")
  (do (println "analyze-alloc:" @failures "FAILED") (System/exit 1)))
