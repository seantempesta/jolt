;; A read scope (zip-file.ss call-with-zipdir-read-scope: the loader opens one around a require)
;; keeps the jar readers it opened until it ends, across a park of the fiber that reads. A park
;; unwinds the fiber's winders on its way to the scheduler, and the scope's cleanup was a plain
;; winder: a fiber parked or preempted mid-read had its reader's port closed under it, and the
;; read it resumed raised "set-port-position!: not permitted on closed port". A Seon fork server
;; reading its deps' jars at boot, a preemptible fiber, died of it.
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def dir (str (System/getProperty "java.io.tmpdir") "/zip-read-scope-park-" (System/nanoTime)))
(.mkdirs (java.io.File. dir))
(def jar (str dir "/a.jar"))
(with-open [z (java.util.zip.ZipOutputStream. (java.io.FileOutputStream. jar))]
  (.putNextEntry z (java.util.zip.ZipEntry. "a.clj"))
  (.write z (.getBytes "(ns a)"))
  (.closeEntry z))

;; inside a scope: the scope's reader for the jar, (park!), then the jar's first four bytes
;; through that same reader as one number (a zip's local header signature, 0x04034b50), or
;; what the read raised
(def read-across
  (scm/eval-string
    "(lambda (path park!)
       (call-with-zipdir-read-scope
         (lambda ()
           (let* ((r (zipdir-scoped-reader (zipdir-scope-readers) (zipdir-for path)))
                  (bv (make-bytevector 4)))
             (park!)
             (guard (e (#t (if (message-condition? e) (condition-message e) e)))
               ((car r) 0 bv 0 4)
               (bytevector-u32-ref bv 0 (endianness little)))))))"))

(def got
  (jolt.host/main-carrier-run!
    (fn []
      [(read-across jar (fn [] nil))
       ;; the fiber parks on a timeout: the scope's winders are unwound and rewound
       (read-across jar (fn [] (a/<!! (a/timeout 1))))])))

(check-eq "a read in a scope, no park" (first got) 0x04034b50)
(check-eq "a read in a scope through the reader it held across a park" (second got) 0x04034b50)

(if (empty? @failures)
  (do (println "ZIP-READ-SCOPE-PARK-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
