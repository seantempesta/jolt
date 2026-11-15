;; A child forked from an armed process arms its own watcher (fibers-async.ss jolt-rearm-after-fork!),
;; so its SIGINFO dump is written while its main thread has interrupts disabled, as its parent's is
;; (siginfo-dump-disabled-test.clj). fork(2) copies no watcher thread: without its own, a forked Seon
;; agent's dump waited for an event check (flow, 2026-11-15). The parent forks with fork-alone, so an
;; armed parent's watcher, parked in sigwait, never holds the fork (no wait, no refusal).
(require '[jolt.host :as h] '[jolt.scheme :as scm])

(def file (str (System/getProperty "java.io.tmpdir") "/jolt-siginfo-child-" (System/currentTimeMillis) ".txt"))

(def child-body
  (scm/eval-string
    "(lambda (path)
       (let ([creat (foreign-procedure \"creat\" (string int) int)]
             [dup2 (foreign-procedure \"dup2\" (int int) int)]
             [close (foreign-procedure \"close\" (int) int)]
             [usleep (foreign-procedure \"usleep\" (unsigned-32) int)]
             [exit (foreign-procedure \"_exit\" (int) void)])
         (let ([fd (creat path #o644)]) (dup2 fd 2) (close fd))
         (disable-interrupts)
         (usleep 2000000)
         (let ([n (let* ([p (open-file-input-port path)] [b (get-bytevector-all p)])
                    (close-port p) (if (eof-object? b) 0 (bytevector-length b)))])
           (exit (if (> n 0) 0 1)))))"))

(def parent-wait
  (scm/eval-string
    "(lambda (pid)
       (let ([kill (foreign-procedure \"kill\" (int int) int)]
             [usleep (foreign-procedure \"usleep\" (unsigned-32) int)]
             [waitpid (foreign-procedure __collect_safe \"waitpid\" (int u8* int) int)]
             [st (make-bytevector 4 0)])
         (usleep 500000)
         (kill pid 29)
         (waitpid pid st 0)
         (bytevector-s32-native-ref st 0)))"))

(let [pid (h/fork-alone 5000)]
  (cond
    (zero? pid) (child-body file)
    (neg? pid) (do (println "siginfo-dump-child: FAIL: fork-alone refused" pid "holders" (h/fork-holders)) (System/exit 1))
    :else (let [status (parent-wait pid)]
            (if (zero? status)
              (println "siginfo-dump-child: PASS (the child wrote its dump with interrupts disabled)")
              (do (println "siginfo-dump-child: FAIL: status" status "dump file:" (slurp file)) (System/exit 1))))))
