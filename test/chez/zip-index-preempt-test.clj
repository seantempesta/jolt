;; A jar's index read (zip-file.ss zipdir-read) keeps its port open when the fiber reading it is
;; preempted: the fiber scheduler's quantum (fibers.ss jolt-fiber-preempt-handler, Chez's engine
;; timer) switches the fiber out through a park escape, which unwinds every plain winder, and
;; zipdir-read's cleanup was one, so the read it resumed raised "set-port-position!: not
;; permitted on closed port". A Seon fork server died of it at boot, indexing its deps' jars.
;; The quantum counts engine ticks, so each quantum lands its preemptions at the same points
;; every run; stepping it from the floor up puts one inside every stretch of the read.
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def dir (str (System/getProperty "java.io.tmpdir") "/zip-index-preempt-" (System/nanoTime)))
(.mkdirs (java.io.File. dir))
(def jar (str dir "/a.jar"))
(with-open [z (java.util.zip.ZipOutputStream. (java.io.FileOutputStream. jar))]
  (dotimes [i 8] (.putNextEntry z (java.util.zip.ZipEntry. (str "e/" i ".clj"))) (.write z (.getBytes (str "(ns e" i ")"))) (.closeEntry z)))

(def set-quantum! (scm/eval-string "(lambda (n) (jolt-fiber-preempt-ticks-set! n))"))
(def default-quantum! (scm/eval-string "(lambda () (jolt-fiber-preempt-ticks-set! #f))"))
(def preempts (scm/eval-string "(lambda () (jolt-fiber-preempts))"))
;; the jar's index read once: its entry count, or the message of what it raised
(def index-count
  (scm/eval-string
    "(lambda (path) (guard (e (#t (if (message-condition? e) (condition-message e) e))) (zipdir-size (zipdir-read path #f))))"))

(defn fiber-go [f] (binding [a/*go-backend* :fiber] (a/go (f))))

(def quanta (range 100 2100 7))
(def run
  (jolt.host/main-carrier-run!
    (fn []
      (let [p0 (preempts)
            answers (vec (for [q quanta]
                           (do (set-quantum! q)
                               (a/<!! (fiber-go #(index-count jar))))))]
        (default-quantum!)
        {:answers answers :preempts (- (preempts) p0)}))))

(check-eq "the quantum preempted the reads (the test reaches the read's middle)" (pos? (:preempts run)) true)
(check-eq "every read, preempted at every quantum stepped through it, indexes the jar's 8 entries"
          (vec (keep (fn [[q a]] (when (not= 8 a) [q a])) (map vector quanta (:answers run))))
          [])
(println "preemptions:" (:preempts run) "over" (count quanta) "quanta")

(if (empty? @failures)
  (do (println "ZIP-INDEX-PREEMPT-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" (subs f 0 (min 400 (count f))))) (flush) (System/exit 1)))
