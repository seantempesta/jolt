;; A quoted reader-literal VALUE compiles (task jolt.backend/eval-emits-a-quoted-uuid-and-inst; tests-4 and context, 2026-11-15:
;; seon.reads-test's #uuid literals failed every test agent's program load, 482 times in a morning, and agents lost turns to it).
;; emit-quoted had cases for the reader FORMS of #uuid, #inst, bigdec and regex, but once a reader had built the value (eval of
;; read text, a program read back as EDN), only maps, vectors, sets and seqs of values had branches: (eval (read-string
;; "(quote {:a #uuid \"...\"})")) threw "unsupported quoted form". Each value now emits the constructor its form emits, a
;; tagged literal its tagged-literal, and a value no reader can build is a compile error naming its type.
;;
;; Run: target/release/jolt run test/chez/emit-quoted-values-test.clj   (prints EMIT-QUOTED-VALUES-TEST OK)
(ns emit-quoted-values-test
  (:require [clojure.edn :as edn]))

(defn- fail! [& parts] (println (apply str "FAIL emit-quoted-values: " parts)) (System/exit 1))

(def cases
  [["uuid" "(quote {:a #uuid \"00000000-0000-0000-0000-000000000001\"})"]
   ["inst" "(quote {:a #inst \"2026-11-15T00:00:00.000-00:00\"})"]
   ["bigdec" "(quote {:a 1.5M})"]
   ["regex" "(quote {:a #\"a+b\"})"]
   ["tagged" "(quote {:a #my/tag [1 2]})"]
   ["nested" "(quote [#{#uuid \"00000000-0000-0000-0000-000000000002\"} (1.25M #inst \"2026-01-01T00:00:00.000-00:00\")])"]])

(defn- same? [a b]
  ;; a regex is = only to itself: compare its source
  (if (instance? java.util.regex.Pattern (:a a)) (= (str (:a a)) (str (:a b))) (= a b)))

(doseq [[label src] cases]
  (let [read (binding [*default-data-reader-fn* tagged-literal] (read-string src))
        want (second read)
        got (try (eval read) (catch Throwable t (fail! label ": eval of the quoted value threw: " (ex-message t))))]
    (when-not (same? want got) (fail! label ": the quoted value came back as " (pr-str got) ", not " (pr-str want)))))

;; the same map as a program read back from EDN (a store's forms are), quoted and compiled
(let [m (edn/read-string {:default tagged-literal} "{:id #uuid \"00000000-0000-0000-0000-000000000003\" :at #inst \"2026-11-15T00:00:00.000-00:00\"}")
      got (eval (list 'quote m))]
  (when-not (= m got) (fail! "edn: the read-back map came back as " (pr-str got))))

;; a value no reader form builds is refused at compile, naming its type
(let [msg (try (eval (list 'quote {:a (Object.)})) nil (catch Throwable t (ex-message t)))]
  (when-not (and msg (re-find #"(?i)type|Object" msg)) (fail! "a value with no reader form was not refused naming its type: " (pr-str msg))))

(println "EMIT-QUOTED-VALUES-TEST OK")
(System/exit 0)
