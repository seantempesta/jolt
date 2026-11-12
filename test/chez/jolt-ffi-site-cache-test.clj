;; A compiled jolt.ffi/cfn site looks its C symbol up once for the process, as a JVM FFI
;; binding resolves at load. The binding a cfn makes is a closure, and a cfn evaluated in a
;; let or a loop makes a fresh one each time: while the resolved procedure was kept in the
;; closure, each fresh one looked the symbol up again (the native handles' dlsym, then the
;; process table's), so a fn that binds its C calls as locals did both on every call. A Seon
;; system spent 46% of a thread in dlsym that way (seon.sqlite's with-row-calls, 2026-11-12).
;; The lookups are counted by wrapping jolt-ffi-dlsym-native (host/chez/java/ffi.ss), which
;; every resolution of a cfn site calls first; nothing is timed for the verdict.
(ns jolt-ffi-site-cache-test)

(require '[jolt.ffi :as ffi] '[jolt.scheme :as scm])

(ffi/load-library)

(def lookups
  (scm/eval-string
    "(let ((n 0) (orig jolt-ffi-dlsym-native))
       (set! jolt-ffi-dlsym-native (lambda (sym) (set! n (+ n 1)) (orig sym)))
       (lambda () n))"))

(defn abs-of
  "abs(x) through a cfn made afresh at each call, as a fn that binds its C calls as locals does."
  [x]
  (let [c-abs (ffi/cfn "abs" [:int] :int)]
    (c-abs x)))

(def n 10000)
(let [before (lookups)
      t0 (System/nanoTime)
      ok? (every? #(= 7 (abs-of %)) (repeat n -7))
      ns-a-call (quot (- (System/nanoTime) t0) n)
      looked (- (lookups) before)]
  (println (str n " calls of a fn that makes its cfn afresh: " looked " lookups, " ns-a-call " ns a call"))
  (when-not ok? (println "FAIL jolt-ffi-site-cache: abs answered wrong") (System/exit 1))
  (when-not (= 1 looked)
    (println "FAIL jolt-ffi-site-cache: the site looked its symbol up" looked "times, not once")
    (System/exit 1)))
(println "JOLT-FFI-SITE-CACHE-TEST OK")
