;; SIGINFO's fiber dump (fibers-async.ss) reaches the process's stderr as it is now: descriptor 2,
;; never the error port the interrupted code has bound, nor the port Jolt chose at boot. A process
;; started with its two streams on one pipe gets one port for both (rt.ss jolt-install-process-streams!),
;; and a Seon agent forked from it moves its descriptor 2 to a file of its own: every dump went to the
;; other stream, the root's log, and a hung agent looked as if it had none (2026-10-28). Run with
;; descriptor 2 joined to 1 (2>&1); this program moves 2 to a file, binds the error port to a string
;; port, sends itself SIGINFO, and reads the file.
(require '[jolt.scheme :as scm])

(def file (str (System/getProperty "java.io.tmpdir") "/jolt-siginfo-dump-" (System/currentTimeMillis) ".txt"))

(def signal-into-a-bound-port!
  (scm/eval-string
    "(lambda (path)
       (let ([creat (foreign-procedure \"creat\" (string int) int)]
             [dup2 (foreign-procedure \"dup2\" (int int) int)]
             [close (foreign-procedure \"close\" (int) int)]
             [kill (foreign-procedure \"kill\" (int int) int)]
             [getpid (foreign-procedure \"getpid\" () int)]
             [captured (open-output-string)])
         ;; creat, not open: open's mode is a variadic argument, which a fixed foreign signature
         ;; passes where arm64 macOS does not read it
         (let ([fd (creat path #o644)])
           (dup2 fd 2) (close fd))
         (parameterize ([current-error-port captured])
           (kill (getpid) 29)
           ;; event checks: the queued signal's handler runs at one of them
           (let loop ([i 0] [acc 0]) (if (< i 2000000) (loop (+ i 1) (+ acc i)) acc)))
         (get-output-string captured)))"))

(defn check [what ok?] (when-not ok? (println "FAIL siginfo-dump:" what) (System/exit 1)))

(let [captured (str (signal-into-a-bound-port! file))
      written (slurp file)]
  (.delete (java.io.File. file))
  (println "descriptor 2 got" (count written) "chars; the bound port" (count captured))
  (check "the dump reached descriptor 2" (.contains written "jolt fiber dump"))
  (check "and not the error port the interrupted code bound" (not (.contains captured "jolt fiber dump"))))
(println "SIGINFO-DUMP-TEST OK")
