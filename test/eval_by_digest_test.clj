;; A top-level form's compiled code is reused by digest (compile-eval.ss jolt-ce-run-emitted): the same forms
;; evaluated in a second fresh process sharing the AOT cache dir are loaded, not compiled. The corpus is real code,
;; clojure.walk, clojure.string and clojure.zip's top-level forms evaluated one by one into namespaces of their own,
;; plus a fn that throws, read with its lines. Three processes on one fresh JOLT_CACHE_DIR:
;;   first    every form compiled;
;;   second   the same forms after the same history: none compiled, every one loaded;
;;   shifted  the same forms after other forms used labels first, so each form's labels carry other numbers: at least
;;            half load (renumbering is what makes them the same text; a form whose text keeps another process-wide
;;            number, a destructuring gensym, compiles);
;; and all three compute the same values and resolve the throw's frame to its line.
;; Estimate (2026-11-14, this corpus, measured warm): a compile is about half a form's eval, so the second process's
;; eval ticks are at most 3/4 of the first's.
;; Run: target/release/jolt run test/eval_by_digest_test.clj
(require '[clojure.java.shell :as sh] '[clojure.string :as str] '[jolt.scheme :as scm])

(defn corpus []
  (vec (for [lib ["walk" "string" "zip"]
             :let [src (slurp (str "stdlib/clojure/" lib ".clj"))]
             form (read-string (str "[" (str/replace src (str "(ns clojure." lib) (str "(ns edigest." lib)) "]"))]
         form)))

(def boom-src "(defn boom [x]\n  (let [y (inc x)]\n    (throw (ex-info \"boom\" {:y y}))))")

(defn child [shifted?]
  (let [counts (scm/eval-string "(lambda () (jolt-vector (vector-ref jolt-eval-compile-counts 0) (vector-ref jolt-eval-compile-counts 1)))")
        thread-bytes (scm/eval-string "(lambda () (jolt-thread-bytes))")
        forms (conj (corpus) (read-string boom-src))]
    (when shifted?
      (dotimes [i 7] (eval (list 'defn (symbol (str "pre" i)) '[a] '(let [b (inc a)] (fn [c] (+ a b c)))))))
    (let [[c0 h0] (counts) b0 (thread-bytes)
          ticks (jolt.host/ticks-of #(binding [*ns* *ns*] (doseq [f forms] (eval f))))
          bytes (- (thread-bytes) b0)
          [c1 h1] (counts)
          line (try (eval '(edigest.zip/boom 1)) nil
                    (catch Exception e
                      (some (fn [el] (when (re-find #"boom" (str (.getClassName el))) (.getLineNumber el)))
                            (.getStackTrace e))))]
      (prn {:forms (count forms) :compiles (- c1 c0) :hits (- h1 h0)
            :ticks-per-form (quot ticks (count forms)) :bytes-per-form (quot bytes (count forms))
            :values [(eval '(edigest.walk/postwalk-replace {:a 1} [:a {:b :a}]))
                     (eval '(edigest.string/capitalize "hello world"))
                     (eval '(edigest.zip/root (edigest.zip/edit (edigest.zip/down (edigest.zip/vector-zip [1 [2 3]])) inc)))]
            :boom-line line}))))

(defn run-child [cache shifted?]
  (let [{:keys [exit out err]} (sh/sh "target/release/jolt" "run" "test/eval_by_digest_test.clj" (if shifted? "shifted" "child")
                                      :env (assoc (into {} (System/getenv)) "JOLT_CACHE_DIR" cache "JOLT_NO_USER_DEPS" "1"))]
    (when-not (zero? exit) (println "FAIL eval-by-digest: a child exited" exit) (println out err) (System/exit 1))
    (read-string (last (str/split-lines out)))))

(defn parent []
  (let [cache (str (System/getProperty "java.io.tmpdir") "/eval-by-digest-" (System/currentTimeMillis))
        first-run (run-child cache false)
        second-run (run-child cache false)
        shifted (run-child cache true)
        n (:compiles first-run)               ; units: an ns form's expansion is several
        fail (fn [why] (println "FAIL eval-by-digest:" why) (System/exit 1))]
    (doseq [[k r] [[:first first-run] [:second second-run] [:shifted shifted]]] (println "eval-by-digest" k r))
    (sh/sh "rm" "-rf" cache)
    (when-not (and (>= n (:forms first-run)) (zero? (:hits first-run)))
      (fail (str "the first process, on an empty cache, compiled " n " and loaded " (:hits first-run))))
    (when-not (and (zero? (:compiles second-run)) (= n (:hits second-run)))
      (fail (str "the second process compiled " (:compiles second-run) " of " n " forms the first had compiled")))
    (when-not (>= (* 2 (:hits shifted)) n)
      (fail (str "after other forms, " (:hits shifted) " of " n " forms loaded: their labels were not renumbered")))
    (when-not (apply = (map :values [first-run second-run shifted])) (fail "the processes computed different values"))
    (when-not (every? #(= 3 (:boom-line %)) [first-run second-run shifted])
      (fail (str "the throw's frame did not resolve to line 3: " (mapv :boom-line [first-run second-run shifted]))))
    (when-not (<= (* 4 (:ticks-per-form second-run)) (* 3 (:ticks-per-form first-run)))
      (fail (str "a loaded form cost " (:ticks-per-form second-run) " ticks, a compiled one " (:ticks-per-form first-run))))
    (println "eval-by-digest OK")
    (System/exit 0)))

(case (first *command-line-args*)
  "child" (child false)
  "shifted" (child true)
  (parent))
