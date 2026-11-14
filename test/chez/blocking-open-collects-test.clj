;; Task jolt.host/a-blocking-call-on-a-main-carrier-parks, its second proof (jolt.host/a-thread-in-a-blocking-call-leaves-
;; the-collector-running). A collection waits for every thread to reach a safe point; a thread pinned in a plain (not
;; collect-safe) C call never does, so the first collection after it hangs the whole process (forkbound's holder in a
;; plain read hung its gate 1,100 s, 2026-11-14). Here one thread opens a FIFO nobody writes, through Jolt's ordinary
;; slurp, and stays waiting; another allocates enough for many collections and forces one more. It must answer while the
;; first still waits; the writer then comes, and the first answers what it wrote. A hang is this test's red (the gate's
;; alarm ends it).
(require '[clojure.java.shell :as shell])

(def dir (str (System/getProperty "java.io.tmpdir") "/blocking-open-collects-" (System/currentTimeMillis)))
(.mkdirs (java.io.File. dir))
(def f (str dir "/f"))
(shell/sh "mkfifo" f)

(defn collect! []
  (dotimes [i 200] (vec (range 20000)))   ; ~200 x 160 KB: collections, each needing every thread at a safe point
  (System/gc)
  :collected)

;; (a) the thread waits in the open: nobody has the other end
(def waiting (future (slurp f)))
(Thread/sleep 100)
(def open-case [(collect!) (not (realized? waiting))])
;; (b) the thread waits in a read: a writer holds the FIFO open and has written nothing yet
(def holder (future (with-open [w (java.io.FileOutputStream. f)]
                      (Thread/sleep 300)
                      (collect!)
                      (.write w (.getBytes "written")))))
(def got (deref waiting 10000 :no-answer))
(deref holder 10000 :no-answer)

(shell/sh "rm" "-rf" dir)
(shutdown-agents)
(let [ok (and (= open-case [:collected true]) (= got "written"))]
  (if ok
    (println "blocking-open-collects: OK")
    (do (println "FAIL: while the open waited" open-case "; then, a collection while the read waited, it read" (pr-str got))
        (System/exit 1))))
