;; clojure.java.shell/sh from a fiber starts no thread: its stdin, stdout and stderr copies run on
;; fibers parked on their pipes (jolt.io-poller), and its wait is the child's exit event. It used to
;; start a future per stream, OS threads the main carrier's loop had to read as "may run"; one that
;; woke the boot fiber and ended inside the loop's decision killed a Seon root at boot (2026-11-09,
;; main-carrier-stuck-test). Here, on a main carrier, the kernel's count of this process's threads
;; is read while sh waits, and every answer is the one a thread's sh gives (stdin, a megabyte each
;; way, bytes, exit status, :dir, :env).
(require '[clojure.java.shell :as shell] '[jolt.fibers :as fibers] '[jolt.scheme :as scm])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def threads
  "This process's OS threads, as the kernel counts them (Mach task_threads; /proc/self/task on Linux)."
  (scm/eval-string
    "(if (file-exists? \"/proc/self/task\")
         (lambda () (length (directory-list \"/proc/self/task\")))
         (let ([tt (foreign-procedure \"task_threads\" (unsigned-32 uptr uptr) int)])
           (lambda ()
             (let ([lp (foreign-alloc 8)] [cp (foreign-alloc 4)])
               (tt (foreign-ref 'unsigned-32 (foreign-entry \"mach_task_self_\") 0) lp cp)
               (let ([n (foreign-ref 'unsigned-32 cp 0)]) (foreign-free lp) (foreign-free cp) n)))))"))

(def big (apply str (repeat 65536 "0123456789abcde\n")))   ; 1 MiB, past any pipe's buffer
(def tmp (str (System/getProperty "java.io.tmpdir")))

(defn answers
  "The answers every case gives, as plain data."
  []
  [(shell/sh "sh" "-c" "cat; echo to-err >&2; exit 3" :in "abc")
   (let [r (shell/sh "sh" "-c" "cat; cat /dev/null >&2" :in big)] [(:exit r) (= big (:out r)) (:err r)])
   (let [r (shell/sh "sh" "-c" "cat >&2" :in big)] [(:exit r) (:out r) (= big (:err r))])
   (vec (:out (shell/sh "printf" "\\001\\002\\377" :out-enc :bytes)))
   (shell/sh "sh" "-c" "pwd -P; echo $SH_TEST_V" :dir tmp :env {"SH_TEST_V" "v1" "PATH" (System/getenv "PATH")})])

(def want
  [{:exit 3 :out "abc" :err "to-err\n"}
   [0 true ""]
   [0 "" true]
   [1 2 -1]
   {:exit 0 :out (str (.getCanonicalPath (java.io.File. tmp)) "\nv1\n") :err ""}])

(def on-carrier
  (jolt.host/main-carrier-run!
    (fn []
      ;; what a first call loads (core.async, the poller) is loaded before the count
      (shell/sh "true")
      (let [before (threads)
            during (fibers/spawn #(do (Thread/sleep 200) (threads)))
            slow (shell/sh "sh" "-c" "sleep 0.6; echo done")
            got (answers)]
        {:before before :during (fibers/join during) :after (threads) :slow slow :got got}))))

(check-eq "a sh on the main carrier starts no thread while it waits" (:during on-carrier) (:before on-carrier))
(check-eq "nor leaves one" (:after on-carrier) (:before on-carrier))
(check-eq "a sh that waits for its child answers" (:slow on-carrier) {:exit 0 :out "done\n" :err ""})
(check-eq "on a fiber, sh answers what the contract says" (:got on-carrier) want)
;; the main carrier has returned: this is an ordinary thread, whose sh copies on futures
(check-eq "on a thread, sh answers the same" (answers) want)

(if (empty? @failures)
  (do (println "MAIN-CARRIER-SH-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
