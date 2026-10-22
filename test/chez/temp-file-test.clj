;; java.io.File/createTempFile gate: a temp file is a path no other process holds. The
;; name was the millisecond and a per-process counter, checked and then created, so
;; processes making their first temp file in one millisecond were handed one path (four
;; test shards started at once, each making a git index there: "index.lock: File
;; exists"). Now the name is the pid and the counter, and the create is exclusive.
;;
;; Run: bin/jolt run test/chez/temp-file-test.clj [JOLT] (prints "TEMP-FILE OK"); JOLT
;; is the runtime the child processes run (default bin/jolt).
(ns temp-file-test
  (:require [clojure.java.shell :as sh] [clojure.string :as str])
  (:import [java.io File]))

(def failures (atom []))
(defn check [label ok] (when-not ok (swap! failures conj label)))

(def jolt (or (first *command-line-args*) "bin/jolt"))
(def dir (str (System/getProperty "java.io.tmpdir") "/jolt-temp-test-" (System/currentTimeMillis)))
(.mkdirs (File. dir))

;; a pre-existing file at what would be the next name is skipped, not overwritten
(let [probe (File/createTempFile "jtp" ".t" (File. dir))
      [_ pid n] (re-find #"jtp(\d+)-(\d+)\.t$" (.getName probe))
      squatter (File. dir (str "jtp" pid "-" (inc (parse-long n)) ".t"))]
  (check "the name is the pid and the counter" (some? pid))
  (spit squatter "kept")
  (let [next (File/createTempFile "jtp" ".t" (File. dir))]
    (check "a pre-existing file at the next name is skipped" (not= (.getPath next) (.getPath squatter)))
    (check "and not overwritten" (= "kept" (slurp squatter)))))

;; eight processes make their temp files in the same millisecond: every path is distinct
(let [script (str dir "/child.clj")
      go (+ (System/currentTimeMillis) 3000)]
  (spit script (str "(loop [] (when (< (System/currentTimeMillis) " go ") (recur)))"
                    "(dotimes [_ 20] (println (.getPath (java.io.File/createTempFile \"jtc\" \".t\" (java.io.File. \"" dir "\")))))"))
  (let [ps (doall (for [_ (range 8)] (future (sh/sh jolt "run" script))))
        paths (mapcat #(str/split-lines (:out @%)) ps)]
    (check (str "every child ran (" (count paths) " paths)") (= 160 (count paths)))
    (check (str "every path distinct (" (count (distinct paths)) " of " (count paths) ")") (= (count paths) (count (distinct paths))))))

(sh/sh "rm" "-rf" dir)
(if (empty? @failures)
  (println "TEMP-FILE OK")
  (do (doseq [f @failures] (println "FAIL" f)) (System/exit 1)))
(shutdown-agents)
