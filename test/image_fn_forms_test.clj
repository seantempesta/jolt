;; *compiler-options* {:image-fn-forms false}: a program that writes no state image
;; compiles without registering its fn literals' sources (backend_scheme.clj
;; fnsrc-regs). Registering renders each anonymous fn's source, reads it back to
;; check it, and emits an image-register-fn-form! per literal plus a maker around
;; it: about a third of a defn's compile allocation. Absent, every literal registers,
;; as before. JOLT_COMPILER_OPTIONS sets *compiler-options*' root at start.
;; Measured 2026-10-23, the defn below: 5.03 MB registering, 3.42 MB not.

(ns image-fn-forms-test (:require [clojure.java.shell :as sh]))

(defn- bytes-now [] (jolt.host/scheme-eval-string "(sstats-bytes (statistics))"))
(def form '(defn summarize [items k]
             (let [xs (filter (fn [m] (contains? m k)) items)
                   ys (map (fn [m] (update m k (fn [v] (if (string? v) (count v) v)))) xs)]
               (reduce (fn [acc m] (merge-with (fn [a b] (+ a b)) acc (select-keys m [k]))) {} ys))))
(defn- compile-bytes [opts]
  (binding [*compiler-options* opts] (eval form))
  (apply min (for [_ (range 5)] (let [b0 (bytes-now)] (binding [*compiler-options* opts] (eval form)) (- (bytes-now) b0)))))

(def failures (atom 0))
(defn- check [what ok?] (when-not ok? (swap! failures inc) (println "FAIL:" what)))

(let [on (compile-bytes nil) off (compile-bytes {:image-fn-forms false})]
  (println "compile bytes: registering" on ", not registering" off)
  (check "not registering allocates at most three quarters of registering" (<= off (* 0.75 on))))
(binding [*compiler-options* {:image-fn-forms false}] (eval form))
(check "the code compiled without registration runs the same"
       (= {:a 5} ((resolve 'image-fn-forms-test/summarize) [{:a "xy"} {:b 1} {:a 3}] :a)))
(let [r (sh/sh "bin/jolt" "-e" "(print (pr-str *compiler-options*))" :env (assoc (into {} (System/getenv)) "JOLT_COMPILER_OPTIONS" "{:image-fn-forms false}"))]
  (check "JOLT_COMPILER_OPTIONS sets *compiler-options*' root" (= "{:image-fn-forms false}" (last (clojure.string/split-lines (:out r))))))

(if (zero? @failures)
  (println "image-fn-forms: OK")
  (do (println "image-fn-forms:" @failures "FAILED") (System/exit 1)))
