;; gate-private-tmpdir.ss — a java.io.tmpdir of the gate's own.
;;
;; unit.edn and corpus.edn rows, and run-inline-body.ss, write fixed names under
;; the temp dir (jolt-io-test.txt, jolt-compilepath-unit/...), so two runs at once -- worktrees
;; and peer sessions run gates side by side -- read and rename each other's
;; files. host-temp-dir reads TMPDIR first on every platform, and a child process
;; inherits it, so pointing it at a fresh directory under the real temp dir
;; isolates the whole run without touching a row.
;;
;; The directory is removed by exit-handler, so a runner that loads this must end
;; through (exit ...), as these three do (gate-summary exits): a --script that falls
;; off its end never calls the handler. jolt-exit-process (the hung-row watchdog's,
;; and System/exit's from any row: it halts as the JVM does, running only the
;; shutdown hooks) skips it too, leaving one directory behind on that path.
;;
;; Load after the runtime (host-temp-dir).

(define (gate-rm-tree p)
  (guard (_ (#t #f))
    (if (file-directory? p #f)                     ; a link is removed, not followed
        (begin
          (guard (_ (#t #f)) (chmod p #o700))      ; rows leave read-only dirs
          (for-each (lambda (n) (gate-rm-tree (string-append p "/" n))) (directory-list p))
          (delete-directory p))
        (delete-file p))))

(define gate-private-tmpdir
  (let ((base (let trim ((b (host-temp-dir)))
                (let ((n (string-length b)))
                  (if (and (> n 1) (memv (string-ref b (- n 1)) '(#\/ #\\)))
                      (trim (substring b 0 (- n 1)))
                      b))))
        (now (current-time)))
    ;; Chez's random state is the same in every process; seed it from the clock
    ;; so two runs started together try different names (mkdir decides anyway).
    (random-seed (+ 1 (mod (+ (time-nanosecond now) (time-second now)) 4294967000)))
    (let try ((k 0))
      (let ((d (format "~a/jolt-gatetmp-~a" base (random 1000000000))))
        (cond ((guard (_ (#t #f)) (mkdir d) #t) (sa-setenv! "TMPDIR" d) d)
              ((< k 20) (try (+ k 1)))
              (else #f))))))                       ; none made: the shared dir, as before

(when gate-private-tmpdir
  (exit-handler
    (let ((exit0 (exit-handler)))
      (lambda args
        (gate-rm-tree gate-private-tmpdir)
        (apply exit0 args)))))
