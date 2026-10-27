;; build-unit-write-test.ss — a split app unit is written once, from memory
;; (build.ss bld-write-unit!), with the same bytes the build wrote before: the unit
;; through a textual port, then read back off the disk for its line-marker table,
;; which was appended after it. The table's offsets are the text's, so a unit
;; holding characters past ASCII (whose UTF-8 bytes outnumber its characters) is
;; the case that would show a table read off the wrong text. A unit with no
;; markers gets no table.
;;
;;   chez --script test/chez/build-unit-write-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")
(load "host/chez/cli-core.ss")
(load "host/chez/png.ss")
(load "host/chez/loader.ss")
(load "host/chez/java/ffi.ss")
(load "host/chez/build.ss")

(define failures 0)
(define (check what ok)
  (unless ok (set! failures (+ failures 1)) (printf "FAIL: ~a\n" what)))

;; the build's write before bld-write-unit!: textual, then the table off the disk
(define (write-unit-before! path text nses)
  (let ((out (open-output-file path 'replace)))
    (put-string out text)
    (close-port out))
  (let ((table (jolt-marker-table (read-file-string path))))
    (when (and (pair? nses) (fx>? (vector-length table) 0))
      (let ((out (open-output-file path 'append)))
        (put-string out ";; === source line markers ===\n")
        (put-string out (string-append
                          "(jolt-register-marker-table! '"
                          (with-output-to-string (lambda () (write nses)))
                          " '" (with-output-to-string (lambda () (write table)))
                          ")\n"))
        (close-port out)))))

(define (file-bytes path)
  (let* ((p (open-file-input-port path)) (bv (get-bytevector-all p)))
    (close-port p)
    (if (eof-object? bv) (bytevector) bv)))

(define dir (string-append "/tmp/jolt-build-unit-write-" (number->string (random 1000000000))))
(mkdir dir)

(define cases
  (list
    (cons "ascii with markers"
          ";; app unit: app.a\n(define jv$app.a/f (void))\n#|L3|#(set! jv$app.a/f (lambda (x) #|L4|#(+ x 1)))\n")
    (cons "past ascii before and between markers"
          ";; app unit: app.é\n(define s \"naïve → ✓\")\n#|L7|#(display s)\n#|L9@app.b/g@12|#(g \"日本\")\n")
    (cons "no markers" ";; app unit: app.c\n(define jv$app.c/x 1)\n")))

(for-each
  (lambda (c)
    (let ((before (string-append dir "/before.ss"))
          (after (string-append dir "/after.ss"))
          (nses (list "app.a")))
      (write-unit-before! before (cdr c) nses)
      (bld-write-unit! after (cdr c) nses)
      (check (string-append (car c) ": the same bytes as the textual write and its read-back table")
             (bytevector=? (file-bytes before) (file-bytes after)))))
  cases)

(let ((after (string-append dir "/after.ss")))
  (bld-write-unit! after (cdr (car cases)) (list "app.a"))
  (check "a unit with markers carries its table"
         (let ((s (utf8->string (file-bytes after))))
           (let loop ((i 0))
             (and (< (+ i 26) (string-length s))
                  (or (string=? (substring s i (+ i 26)) "(jolt-register-marker-tabl")
                      (loop (+ i 1))))))))

(for-each (lambda (f) (delete-file (string-append dir "/" f))) (directory-list dir))
(delete-directory dir)

(if (= failures 0)
    (begin (display "BUILD-UNIT-WRITE OK\n") (exit 0))
    (begin (printf "BUILD-UNIT-WRITE FAILED (~a)\n" failures) (exit 1)))
