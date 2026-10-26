;; A jar's index read (zip-file.ss zipdir-read) keeps its port open when its body is escaped and
;; resumed: an engine's fuel running out mid-read (Chez's preemption, which Jolt's fiber
;; scheduler drives) escapes the body and unwinds every plain winder, and zipdir-read's cleanup
;; was one, so the read it resumed raised "set-port-position!: not permitted on closed port". A
;; Seon fork server died of it at boot, indexing its deps' jars. Fuel is deterministic: each fuel
;; from 1 up lands the expiry at the same point every run, and stepping it through the read puts
;; one inside every stretch of it.
(require '[jolt.scheme :as scm])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def dir (str (System/getProperty "java.io.tmpdir") "/zip-index-engine-" (System/nanoTime)))
(.mkdirs (java.io.File. dir))
(def jar (str dir "/a.jar"))
(with-open [z (java.util.zip.ZipOutputStream. (java.io.FileOutputStream. jar))]
  (dotimes [i 8] (.putNextEntry z (java.util.zip.ZipEntry. (str "e/" i ".clj"))) (.write z (.getBytes (str "(ns e" i ")"))) (.closeEntry z)))

;; for each fuel 1..n: (zipdir-read jar #f) in an engine with that fuel, resumed with ample fuel
;; each time it expires. Answers "N|BAD": N the fuels whose engine expired at least once, BAD each
;; fuel whose read did not index the jar's 8 entries with what it answered (empty when none)
(def by-fuel
  (scm/eval-string
    "(lambda (path n)
       (let loop ((f 1) (expired-runs 0) (bad '()))
         (if (> f n)
             (format \"~a|~a\" expired-runs (apply string-append (reverse bad)))
             (let* ((expired #f)
                    (r (let run ((eng (make-engine (lambda () (guard (e (#t (if (message-condition? e) (condition-message e) e)))
                                                                (zipdir-size (zipdir-read path #f))))))
                                 (fuel f))
                         (eng fuel (lambda (left v) v) (lambda (next) (set! expired #t) (run next 100000))))))
               (loop (+ f 1) (if expired (+ expired-runs 1) expired-runs)
                     (if (eqv? r 8) bad (cons (format \"fuel ~a: ~a; \" f r) bad)))))))"))

(def fuels 400)
(def r (by-fuel jar fuels))
(def expired-runs (parse-long (first (clojure.string/split r #"\|" 2))))
(def bad (second (clojure.string/split r #"\|" 2)))

(check-eq "some fuel expires the engine inside the read (the test reaches the read's middle)"
          (pos? expired-runs) true)
(check-eq "every fuel's read, its body escaped and resumed, indexes the jar's 8 entries" bad "")
(println "fuels that expired mid-read:" expired-runs "of" fuels)

(if (empty? @failures)
  (do (println "ZIP-INDEX-ENGINE-EXPIRY-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" (subs f 0 (min 400 (count f))))) (flush) (System/exit 1)))
