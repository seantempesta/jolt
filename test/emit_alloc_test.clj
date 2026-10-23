;; Emitting a string literal allocates its text, not a string per character: what
;; emitting a form allocates grows with the literal's characters at a ceiling, as
;; the slope between two sizes (n and 10n) of one shape, in bytes. Bytes are
;; deterministic where time is not: each size is the least of several runs, and
;; the slope cancels the fixed per-form cost.
;;
;; chez-str-lit built a one-character string per character (char-escape's
;; (str (char cp))) and joined them with apply str over a lazy seq; munge-chars did
;; the same for a name holding a character to escape. Docstrings are long, so on
;; 400 forms of a downstream program (Seon's src) this was about half of what
;; emit-top-form allocated: 1,159 MB -> 593 MB with the literal written in one
;; indexed pass (output byte-identical). Measured 2026-10-23, before -> after
;; (B a character): a printable character 1,296 -> 256;
;; an escaped one 1,656 -> 340, in the default compiler options, where
;; the fn-source registration escapes the literal three more times than the code
;; does. Each ceiling is the after with 25% room.

(ns emit-alloc-test
  (:require [jolt.analyzer :as ana] [jolt.passes] [jolt.backend-scheme :as be]))

(defn- bytes-now [] (jolt.host/scheme-eval-string "(sstats-bytes (statistics))"))

(defn- emit-bytes
  "The least bytes emitting form allocated over a few runs."
  [form]
  (let [ctx (jolt.host/ctx-for-ns "emit-alloc-test")
        node (jolt.passes/run-passes (ana/analyze ctx form) ctx)]
    (be/emit-top-form node)
    (apply min (for [_ (range 5)]
                 (let [b0 (bytes-now)] (be/emit-top-form node) (- (bytes-now) b0))))))

(defn- slope
  "Bytes per unit of shape f between n and 10n units."
  [f n]
  (/ (double (- (emit-bytes (f (* 10 n))) (emit-bytes (f n)))) (* 9 n)))

(def shapes
  "Each shape of n units, and its ceiling in bytes a unit."
  [["string literal, a printable character"
    (fn [n] (list 'fn* [] (apply str (repeat n "a")))) 320]
   ["string literal, a character to escape (newline)"
    (fn [n] (list 'fn* [] (apply str (repeat n "\n")))) 420]])

(def failures (atom 0))
(doseq [[what f ceiling] shapes]
  (let [s (slope f 200)]
    (println (format "%-48s %8.0f B a unit (ceiling %d)" what s ceiling))
    (when (> s ceiling)
      (swap! failures inc)
      (println "FAIL:" what "allocates" (long s) "B a unit, past" ceiling))))
;; ...and what it emits is unchanged: Chez's escapes for the named characters, a
;; hex escape outside printable ASCII, and every other character as itself.
(doseq [[what got want]
        [["a string literal round-trips" (eval (list 'identity "a\"b\\c\nd\te\rf é ☃ \u0001")) "a\"b\\c\nd\te\rf é ☃ \u0001"]
         ["a local whose name needs munging" (let [a|b 1 c#d 2] (+ a|b c#d)) 3]]]
  (when-not (= want got)
    (swap! failures inc)
    (println "FAIL:" what "- want" (pr-str want) "got" (pr-str got))))
(if (zero? @failures)
  (println "emit-alloc: OK")
  (do (println "emit-alloc:" @failures "FAILED") (System/exit 1)))
