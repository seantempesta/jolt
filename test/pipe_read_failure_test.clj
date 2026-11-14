;; A child's pipe whose read fails is an error naming its errno, never the end of the output: process.ss's
;; pipe port answered 0 (end of stream) for any errno but EINTR and EAGAIN, and read errno in a later call,
;; which a collection or another fiber may have overwritten, so an EAGAIN on a pipe the child had not yet
;; written could end its output early (Seon's shell: `git status` exit 0 with no output, 2026-11-12). The
;; read's errno is now captured at its own call (rt.ss jolt-foreign-proc-blocking-errno). Here the port's
;; descriptor is closed under it, so read(2) fails with EBADF: the read must raise, naming errno 9.
;; Run: target/release/jolt run test/pipe_read_failure_test.clj
(require '[jolt.scheme :as scm] '[clojure.java.shell :as sh])
;; process.ss is loaded lazily, at the first process
(sh/sh "true")
(def read-closed
  (scm/eval-string
    "(lambda ()
       (let ([fds (make-bytevector 8 0)]
             [pipe (foreign-procedure \"pipe\" (u8*) int)]
             [close (foreign-procedure \"close\" (int) int)])
         (pipe fds)
         (let* ([rd (bytevector-s32-native-ref fds 0)] [wr (bytevector-s32-native-ref fds 4)]
                [p (proc-fd-input-port rd)])
           (close rd)
           (close wr)
           (guard (e (#t e)) (get-u8 p)))))"))
(let [v (read-closed)
      msg (when (instance? Throwable v) (ex-message v))]
  (println "pipe-read-failure:" (or msg (str "no error, read answered " (pr-str v))))
  (when-not (and (string? msg) (.contains ^String msg "errno 9"))
    (println "FAIL pipe-read-failure: a read that failed with EBADF did not raise an error naming errno 9")
    (System/exit 1))
  (println "pipe-read-failure OK")
  (System/exit 0))
