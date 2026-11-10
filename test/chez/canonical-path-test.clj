;; java.io.File/getCanonicalPath gate — it is realpath(3), not "make it
;; absolute": symlinks, "." and ".." all resolve. The containment check every
;; Java program writes,
;;
;;   (.startsWith (.getCanonicalPath child) (.getCanonicalPath root))
;;
;; is only a check at all if links resolve; when getCanonicalPath merely
;; absolutized, a symlink inside a served root passed it while pointing
;; anywhere on the filesystem.
;;
;; Run: bin/jolt run test/chez/canonical-path-test.clj (smoke.sh greps for
;; "CANONICAL-PATH OK").
(ns canonical-path-test
  (:import [java.io File]
           [java.nio.file Files Path Paths LinkOption]))

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(defn- path ^Path [s] (Paths/get (str s) (into-array String [])))
(defn- real-path [s] (str (.toRealPath (path s) (into-array LinkOption []))))

(def root (str (System/getProperty "java.io.tmpdir")
               "/jolt-canon-" (System/currentTimeMillis)))
(def inside (str root "/inside"))
(def outside (str root "/outside"))

(.mkdirs (File. inside))
(.mkdirs (File. outside))
(spit (str outside "/secret.txt") "outside")
(spit (str inside "/plain.txt") "inside")

;; a link INSIDE the tree pointing OUT of it — the case a path check exists for
(Files/createSymbolicLink (path (str inside "/escape.txt"))
                          (path (str outside "/secret.txt"))
                          (into-array java.nio.file.attribute.FileAttribute []))
;; and a link to a directory, so the walk has to resolve an interior component
(Files/createSymbolicLink (path (str inside "/out-dir"))
                          (path outside)
                          (into-array java.nio.file.attribute.FileAttribute []))

(def canon-root (.getCanonicalPath (File. inside)))

;; --- symlinks resolve --------------------------------------------------------

(check "link resolves to its target"
       (.getCanonicalPath (File. (str inside "/escape.txt")))
       (real-path (str outside "/secret.txt")))

(check "link is not reported as itself"
       (= (.getCanonicalPath (File. (str inside "/escape.txt")))
          (.getAbsolutePath (File. (str inside "/escape.txt"))))
       false)

(check "interior link component resolves"
       (.getCanonicalPath (File. (str inside "/out-dir/secret.txt")))
       (real-path (str outside "/secret.txt")))

;; the whole point: a containment check written the usual way now catches it
(check "escape fails a containment check"
       (.startsWith (.getCanonicalPath (File. (str inside "/escape.txt")))
                    (str canon-root "/"))
       false)
(check "a real file passes the same check"
       (.startsWith (.getCanonicalPath (File. (str inside "/plain.txt")))
                    (str canon-root "/"))
       true)

;; --- agreement with java.nio, which already resolved ------------------------

(check "agrees with Path/toRealPath"
       (.getCanonicalPath (File. (str inside "/escape.txt")))
       (real-path (str inside "/escape.txt")))

(check "getCanonicalFile agrees with getCanonicalPath"
       (.getPath (.getCanonicalFile (File. (str inside "/escape.txt"))))
       (.getCanonicalPath (File. (str inside "/escape.txt"))))

;; --- "." and ".." ------------------------------------------------------------

(check "dot segments fold"
       (.getCanonicalPath (File. (str inside "/./plain.txt")))
       (str canon-root "/plain.txt"))

(check "dotdot folds"
       (.getCanonicalPath (File. (str inside "/sub/../plain.txt")))
       (str canon-root "/plain.txt"))

;; a relative path is still resolved against user.dir, as before
(check "relative path is absolute"
       (.startsWith (.getCanonicalPath (File. "project.clj")) "/")
       true)

;; --- paths that do not exist -------------------------------------------------
;; the JVM canonicalizes these too, resolving as far as it can rather than
;; throwing: realpath(3) fails on ENOENT, so the tail is re-attached by hand.

(check "missing leaf still canonicalizes"
       (.getCanonicalPath (File. (str inside "/nope.txt")))
       (str canon-root "/nope.txt"))

(check "missing directories still canonicalize"
       (.getCanonicalPath (File. (str inside "/no/such/dir/file.txt")))
       (str canon-root "/no/such/dir/file.txt"))

(check "missing path under a link resolves the link"
       (.getCanonicalPath (File. (str inside "/out-dir/nope.txt")))
       (str (real-path outside) "/nope.txt"))

(check "dotdot folds in a missing tail"
       (.getCanonicalPath (File. (str inside "/no/such/../dir/f.txt")))
       (str canon-root "/no/dir/f.txt"))

(check "root canonicalizes to itself" (.getCanonicalPath (File. "/")) "/")

;; --- cleanup + report --------------------------------------------------------

;; --- a path that can never name a file is refused, not approximated ----------
;; getCanonicalPath answers a best-effort path when realpath fails, which is
;; what the JVM does for a path that merely does not exist yet. It is wrong for
;; a failure meaning the path can NEVER name a file: the JVM raises, and
;; answering a string lets a path that cannot be opened travel on as though it
;; could (jolt#1094).
;;
;; Every expectation below, message text included, was read off JVM Clojure
;; 1.12 rather than chosen. The split the JVM draws is by errno: ENOENT,
;; ENOTDIR and EACCES answer best-effort; ELOOP and ENAMETOOLONG raise.

(defn- canon [p]
  (try (.getCanonicalPath (File. (str p)))
       (catch java.io.IOException e (.getMessage e))))

;; A Java String holds a NUL; a C path cannot. All three placements.
(check "an embedded NUL is refused" (canon (str "/tmp/a" (char 0) "b")) "Invalid file path")
(check "a trailing NUL is refused"  (canon (str "/tmp/x" (char 0)))     "Invalid file path")
;; A LEADING NUL mattered most: it truncated the path to empty, so the answer
;; was the process's own working directory — a caller's path silently becoming
;; somewhere else entirely.
(check "a leading NUL is refused"   (canon (str (char 0) "/tmp/x"))     "Invalid file path")

;; The NUL check belongs to the canonicalising route only. exists answers false
;; there rather than raising, and getAbsolutePath hands the NUL back — both are
;; the JVM's behaviour and neither may start throwing.
(check "exists with a NUL still answers false"
       (.exists (File. (str "/tmp/a" (char 0) "b"))) false)
(check "getAbsolutePath keeps the NUL"
       (count (.getAbsolutePath (File. (str "/tmp/a" (char 0) "b")))) 8)
;; ...and the answer may not depend on what the path's text before the NUL
;; names: a C path stops there, so asking the OS asked about that file instead.
;; Every File query takes the JVM's invalid-path answer, against a file and a
;; directory that do exist ahead of the NUL.
(let [f (File. (str inside "/plain.txt" (char 0) "x"))
      d (File. (str inside (char 0) "x"))]
  (check "a NUL past an existing file: exists"       (.exists f) false)
  (check "a NUL past an existing file: isFile"       (.isFile f) false)
  (check "a NUL past an existing file: canRead"      (.canRead f) false)
  (check "a NUL past an existing file: length"       (.length f) 0)
  (check "a NUL past an existing file: lastModified" (.lastModified f) 0)
  (check "a NUL past an existing file: delete"       (.delete f) false)
  (check "a NUL past an existing file: renameTo"
         (.renameTo f (File. (str inside "/renamed.txt"))) false)
  (check "a NUL past an existing file: createNewFile"
         (try (.createNewFile f) (catch java.io.IOException e (.getMessage e)))
         "Invalid file path")
  (check "a NUL past an existing directory: isDirectory" (.isDirectory d) false)
  (check "a NUL past an existing directory: list"        (.list d) nil)
  (check "a NUL past an existing directory: listFiles"   (.listFiles d) nil)
  (check "a NUL past an existing directory: mkdirs"      (.mkdirs d) false)
  (check "a NUL path cannot be slurped"
         (try (slurp f) (catch java.io.FileNotFoundException e (.getMessage e)))
         "Invalid file path")
  (check "a NUL path cannot be spit to"
         (try (spit f "clobbered") (catch java.io.FileNotFoundException e (.getMessage e)))
         "Invalid file path")
  (check "a NUL path cannot be opened as a stream"
         (try (.close (java.io.FileInputStream. f)) :opened
              (catch java.io.FileNotFoundException e (.getMessage e)))
         "Invalid file path")
  (check "the file a NUL path stopped at is untouched"
         (slurp (str inside "/plain.txt")) "inside"))

;; A symlink cycle, and the distinction that makes it subtle: the JVM raises
;; when it had to WALK THROUGH the loop, and answers when the loop is the final
;; component it never had to resolve.
(let [d (str root "/loop")]
  (.mkdirs (File. d))
  (let [a (str d "/a") b (str d "/b")]
    (Files/createSymbolicLink (path a) (path b) (into-array java.nio.file.attribute.FileAttribute []))
    (Files/createSymbolicLink (path b) (path a) (into-array java.nio.file.attribute.FileAttribute []))
    (check "a traversed symlink loop raises"
           (canon (str a "/db")) "Too many levels of symbolic links")
    (check "a deeper traversal raises too"
           (canon (str a "/x/y")) "Too many levels of symbolic links")
    (check "the loop as the final component still answers"
           (canon a) (str (real-path d) "/a"))
    (try (.delete (File. a)) (catch Throwable _ nil))
    (try (.delete (File. b)) (catch Throwable _ nil)))
  ;; a dangling link is not a loop: it answers, like a missing path
  (let [dang (str d "/dangling")]
    (Files/createSymbolicLink (path dang) (path (str d "/nothing")) (into-array java.nio.file.attribute.FileAttribute []))
    (check "a dangling symlink answers rather than raising"
           (canon dang) (str (real-path d) "/dangling"))
    (try (.delete (File. dang)) (catch Throwable _ nil)))
  (try (.delete (File. d)) (catch Throwable _ nil)))

;; An over-long component is the same shape: it raises where it must be
;; traversed, and answers where it is the tail.
(let [n500 (apply str (repeat 500 "n"))]
  (check "an over-long intermediate component raises"
         (canon (str root "/" n500 "/x")) "File name too long")
  (check "an over-long final component answers"
         (canon (str root "/" n500)) (str (real-path root) "/" n500)))

;; and the ordinary failures keep their best-effort answer
(check "a missing intermediate still answers"
       (canon (str root "/no-such-zzz/file")) (str (real-path root) "/no-such-zzz/file"))
(let [f (str root "/regular.txt")]
  (spit f "x")
  (check "a path through a regular file still answers"
         (canon (str f "/x")) (str (real-path root) "/regular.txt/x"))
  (try (.delete (File. f)) (catch Throwable _ nil)))

;; getCanonicalFile is the same contract, so it raises where the string form does
(check "getCanonicalFile refuses a NUL too"
       (try (.getPath (.getCanonicalFile (File. (str "/tmp/a" (char 0) "b"))))
            (catch java.io.IOException e (.getMessage e)))
       "Invalid file path")

(doseq [f [(str inside "/escape.txt") (str inside "/out-dir") (str inside "/plain.txt")
           (str outside "/secret.txt") inside outside root]]
  (try (.delete (File. f)) (catch Throwable _ nil)))

(if (seq @failures)
  (do (println "CANONICAL-PATH FAILURES:")
      (doseq [f @failures] (println " " f))
      (System/exit 1))
  (println "CANONICAL-PATH OK"))
