;; jolt.host/start-file-reads! and take-file-reads!: the paths every user-facing read announced
;; between them, from any thread, sorted; none open answers nil; a second start discards the first.
(require '[clojure.java.io :as io])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def dir (str (.replaceAll (System/getProperty "java.io.tmpdir") "/+$" "") "/file-reads-test-" (System/nanoTime)))
(.mkdirs (io/file dir))
(def a (str dir "/a.txt")) (def b (str dir "/b.txt")) (def c (str dir "/c.txt"))
(doseq [f [a b c]] (spit f "x"))

(check-eq "none open" (jolt.host/take-file-reads!) nil)
(jolt.host/start-file-reads!)
(slurp a)
@(future (slurp b))
(with-open [r (io/reader c)] (.read r))
(vec (file-seq (io/file dir)))
(check-eq "every read, on any thread, sorted"
          (vec (filter #(.startsWith ^String % dir) (jolt.host/take-file-reads!)))
          (vec (sort [dir a b c])))
(check-eq "closed by take" (jolt.host/take-file-reads!) nil)
(jolt.host/start-file-reads!)
(slurp a)
(jolt.host/start-file-reads!)
(slurp b)
(check-eq "a second start discards the first" (vec (jolt.host/take-file-reads!)) [b])
(slurp a)
(check-eq "no log open: reads announce nothing" (jolt.host/take-file-reads!) nil)

(if (seq @failures)
  (do (doseq [f @failures] (println "FAIL" f)) (System/exit 1))
  (println "file-reads-test OK"))
