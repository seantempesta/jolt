;; A read scope (zip-file.ss call-with-zipdir-read-scope: the loader opens one around a require)
;; keeps the jar readers it opened until it ends, across a park of the fiber that reads; and a
;; copy (clojure.java.io/copy, Files/copy) keeps its source and its file open across a park of
;; the fiber copying, as when its source is a pipe or a socket. A park
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


(defn parking-stream
  "An InputStream of bytes bs that parks its fiber once, after its first half."
  [bs]
  (let [i (atom 0) parked (atom false) n (count bs)]
    (proxy [java.io.InputStream] []
      (read
        ([] (let [k @i] (if (>= k n) -1 (do (swap! i inc) (bit-and (nth bs k) 0xff)))))
        ([b off len]
         (when (and (not @parked) (>= @i (quot n 2))) (reset! parked true) (a/<!! (a/timeout 1)))
         (let [k @i]
           (if (>= k n)
             -1
             (let [m (min len (- n k) (quot n 4))]
               (dotimes [j m] (aset b (+ off j) (byte (nth bs (+ k j)))))
               (swap! i + m)
               m))))))))

(def payload (vec (.getBytes (apply str (repeat 200 "copied across a park\n")))))

(def got-copies
  ;; one main carrier a process: the scope's reads and the copies on it
  (jolt.host/main-carrier-run!
    (fn []
      [[(read-across jar (fn [] nil))
        ;; the fiber parks on a timeout: the scope's winders are unwound and rewound
        (read-across jar (fn [] (a/<!! (a/timeout 1))))]
      (let [to-bytes (java.io.ByteArrayOutputStream.)
            f1 (str dir "/io-copy.txt") f2 (str dir "/files-copy.txt")
            run (fn [thunk] (try (thunk) :copied (catch Throwable t (or (ex-message t) (str t)))))]
        [(run #(clojure.java.io/copy (parking-stream payload) to-bytes))
         (= payload (vec (.toByteArray to-bytes)))
         (run #(clojure.java.io/copy (parking-stream payload) (java.io.File. f1)))
         (= payload (vec (.getBytes (slurp f1))))
         (run #(java.nio.file.Files/copy (parking-stream payload) (java.nio.file.Paths/get f2 (into-array String [])) (into-array java.nio.file.CopyOption [])))
         (= payload (vec (.getBytes (slurp f2))))])])))
(def got (first got-copies))
(def copies (second got-copies))

(check-eq "io/copy into a stream and into a file, and Files/copy, from a source that parks" copies [:copied true :copied true :copied true])

(check-eq "a read in a scope, no park" (first got) 0x04034b50)
(check-eq "a read in a scope through the reader it held across a park" (second got) 0x04034b50)

(if (empty? @failures)
  (do (println "ZIP-READ-SCOPE-PARK-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
