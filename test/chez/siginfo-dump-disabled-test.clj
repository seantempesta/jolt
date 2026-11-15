;; SIGINFO's fiber dump is written while the main thread cannot run a Scheme handler: a thread whose
;; interrupts are disabled reaches no event check, so a dump that waits for one never comes. A hung
;; Seon test runner dumped 0 bytes this way (2026-11-15, 5 of 5 overruns). The shutdown watcher, the
;; thread parked in sigwait (concurrency.ss jolt-arm-shutdown!), takes SIGINFO and writes the dump
;; itself. This program moves descriptor 2 to a file, disables its interrupts, sends itself SIGINFO,
;; sleeps in C for 1.5 s, and reads the file before it enables them again.
(require '[jolt.scheme :as scm])

(def file (str (System/getProperty "java.io.tmpdir") "/jolt-siginfo-disabled-" (System/currentTimeMillis) ".txt"))

(def dump-while-disabled
  (scm/eval-string
    "(lambda (path)
       (let ([creat (foreign-procedure \"creat\" (string int) int)]
             [dup2 (foreign-procedure \"dup2\" (int int) int)]
             [close (foreign-procedure \"close\" (int) int)]
             [kill (foreign-procedure \"kill\" (int int) int)]
             [getpid (foreign-procedure \"getpid\" () int)]
             [usleep (foreign-procedure \"usleep\" (unsigned-32) int)])
         (let ([fd (creat path #o644)])
           (dup2 fd 2) (close fd))
         (disable-interrupts)
         (kill (getpid) 29)
         (usleep 1500000)
         (let ([n (let ([p (open-file-input-port path)])
                    (let ([n (bytevector-length (get-bytevector-all-or-empty p))]) (close-port p) n))])
           (enable-interrupts)
           n)))"))

(scm/eval-string "(define (get-bytevector-all-or-empty p) (let ([b (get-bytevector-all p)]) (if (eof-object? b) #vu8() b)))")

(let [n (dump-while-disabled file)
      text (slurp file)]
  (if (and (pos? n) (re-find #"jolt fiber dump" text))
    (println "siginfo-dump-disabled: PASS (" n "bytes written while the main thread had interrupts disabled)")
    (do (println "siginfo-dump-disabled: FAIL: nothing was written while interrupts were disabled (" n "bytes)")
        (System/exit 1))))
