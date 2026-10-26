;; jolt.host/zip-archive? answers about the file: false for a path that holds no file or a file
;; that is not a whole archive, true for a whole one. A failure that says nothing about the file
;; (the process out of descriptors, an I/O error) raises to the caller with its own cause.
;; Unfixed, every failure answered false: a Seon fork server whose open failed was told an intact
;; jar "is not a whole zip archive (a cut download)", and the cause was lost (2026-10-26).
(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def dir (str (System/getProperty "java.io.tmpdir") "/zip-archive-errors-" (System/nanoTime)))
(.mkdirs (java.io.File. dir))
(defn jar! [name]
  (let [path (str dir "/" name)]
    (with-open [z (java.util.zip.ZipOutputStream. (java.io.FileOutputStream. path))]
      (.putNextEntry z (java.util.zip.ZipEntry. "a.clj"))
      (.write z (.getBytes "(ns a)"))
      (.closeEntry z))
    path))

(check-eq "a whole archive" (jolt.host/zip-archive? (jar! "whole.jar")) true)
(check-eq "a path that holds no file" (jolt.host/zip-archive? (str dir "/none.jar")) false)
(spit (str dir "/text.jar") "this is not a zip file\n")
(check-eq "a file that is not an archive" (jolt.host/zip-archive? (str dir "/text.jar")) false)

;; a whole archive asked while the process has no descriptor left (made before the exhaustion,
;; asked for the first time after it, so no cached index answers; its make target runs it under
;; `ulimit -n 256`, so the descriptors run out long before the cap)
(def fresh (jar! "fresh.jar"))
(def held (loop [acc []]
            (let [s (try (java.io.FileInputStream. fresh) (catch Throwable _ nil))]
              (if (and s (< (count acc) 100000)) (recur (conj acc s)) acc))))
(def answer (try (jolt.host/zip-archive? fresh) (catch Throwable t (str (.getName (class t)) " " (ex-message t)))))
(run! #(.close %) held)
(check-eq "a whole archive the process cannot open raises its open's cause"
          (if (and (string? answer) (re-find #"Too many open files" answer)) :raised answer) :raised)
(check-eq "and once a descriptor is free it is a whole archive" (jolt.host/zip-archive? fresh) true)

(if (empty? @failures)
  (do (println "ZIP-ARCHIVE-ERRORS-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
