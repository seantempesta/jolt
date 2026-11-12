;; A C entry named by a literal is looked up once, never on each call of the function that
;; uses it: (foreign-entry? "f"), (foreign-entry "f"), (foreign-procedure "f" ...) and
;; (jolt-foreign-proc-safe "f" ...) each look "f" up through dlsym over every loaded object
;; when they are evaluated, so one inside a function body repeated the lookup on every call:
;; (System/getenv) looked up _NSGetEnviron twice a call, and a hung Seon system spent 46% of
;; a thread in dlsym (2026-11-12). A lookup is kept when it is evaluated outside any
;; function, inside the value of a set! (a memo), or inside a resolver handed to
;; sa-entry-once (scheme-adapter-runtime.ss). A lookup of a name the function was given
;; (sa-foreign-entry?, ffi-find-symbol) is that function's job and is not counted.
;; Reads every host/chez file the runtime and the build load, as Chez reads them.
;;   chez --script test/chez/foreign-lookup-once-test.ss
(import (chezscheme))

(define lookups '(foreign-procedure foreign-entry? foreign-entry jolt-foreign-proc-safe))
(define (strip a) (if (annotation? a) (annotation-expression a) a))
(define (items x) (let loop ((x x) (acc '())) (if (pair? x) (loop (cdr x) (cons (car x) acc)) (reverse acc))))
(define (line-of text off)
  (let loop ((i 0) (n 1))
    (if (>= i off) n (loop (+ i 1) (if (char=? (string-ref text i) #\newline) (+ n 1) n)))))
(define unkept '())

(define (scan file)
  (let* ((text (call-with-input-file file get-string-all))
         (sfd (make-source-file-descriptor file (open-bytevector-input-port (string->utf8 text))))
         (ip (open-string-input-port text)))
    ;; in-fn: inside a function body; kept: inside a set!'s value or an sa-entry-once resolver
    (define (walk a in-fn kept fn)
      (let ((x (strip a)))
        (when (pair? x)
          (let ((hd (strip (car x))))
            (when (and (memq hd lookups) in-fn (not kept)
                       (exists (lambda (y) (string? (strip y))) (cdr (items x))))
              (set! unkept (cons (format "~a:~a: ~a in ~a" file
                                         (line-of text (source-object-bfp (annotation-source a))) hd fn)
                                 unkept)))
            (cond
              ((memq hd '(quote quasiquote define-syntax syntax-rules)) (void))
              ((memq hd '(lambda case-lambda))
               (for-each (lambda (y) (walk y #t kept fn)) (cdr (items x))))
              ((memq hd '(set! sa-entry-once))
               (for-each (lambda (y) (walk y in-fn #t fn)) (cdr (items x))))
              ((and (eq? hd 'define) (pair? (cdr x)) (pair? (strip (cadr x))))
               (for-each (lambda (y) (walk y #t kept (strip (car (strip (cadr x)))))) (cddr (items x))))
              ((and (eq? hd 'define) (pair? (cdr x)))
               (for-each (lambda (y) (walk y in-fn kept (if in-fn fn (strip (cadr x))))) (cdr (items x))))
              (else (for-each (lambda (y) (walk y in-fn kept fn)) (items x))))))))
    (let loop ((bfp 0))
      (let-values (((a next) (get-datum/annotations ip sfd bfp)))
        (unless (eof-object? a) (walk a #f #f 'toplevel) (loop next))))))

(define files
  (filter (lambda (f) (let ((n (string-length f)))
                        (and (> n 3) (string=? ".ss" (substring f (- n 3) n)))))
          (append (map (lambda (f) (string-append "host/chez/" f)) (directory-list "host/chez"))
                  (map (lambda (f) (string-append "host/chez/java/" f)) (directory-list "host/chez/java")))))
(for-each scan (list-sort string<? files))
(for-each (lambda (l) (printf "FAIL foreign-lookup-once: ~a, in a function body and not kept\n" l)) (reverse unkept))
(printf "foreign-lookup-once: ~a files, ~a lookups of a literal name in a function body and not kept\n"
        (length files) (length unkept))
(exit (if (null? unkept) 0 1))
