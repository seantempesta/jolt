;; run-interruptible's tick budget: runaway work bounded by the work it does. Run from
;; repo root:
;;   chez --script test/chez/tick-budget-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0)
(define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred
    (set! fails (+ fails 1))
    (printf "FAIL: ~a~n" name)))

(define (ex-data-of e)
  (let ((v (jolt-unwrap-throw e)))
    (and (jolt-ex-info-record? v) (jolt-ex-info-record-data v))))
(define (ticks-spent e)
  (let ((d (ex-data-of e))) (and d (jolt-get d jolt-kw-ticks-spent #f))))

;; 1. a tight loop that allocates nothing stops once it has run its budget
(define budget 1000000)
(let ((r (guard (e (#t e))
           (jolt-run-interruptible (jolt-make-interrupt) (lambda () (let loop () (loop))) budget))))
  (ok "an endless loop is stopped by its budget" (ticks-spent r))
  (ok "it ran its budget and not more than one poll past it"
      (let ((n (ticks-spent r))) (and n (>= n budget) (< n (+ budget 100000)))))
  (ok "the stop is an interrupt"
      (eq? #t (jolt-get (ex-data-of r) jolt-kw-interrupted #f))))

;; 2. the body's own catch-all cannot swallow the stop
(let ((r (guard (e (#t e))
           (jolt-run-interruptible (jolt-make-interrupt)
                                   (lambda () (guard (e (#t 'swallowed)) (let loop () (loop))))
                                   budget))))
  (ok "a guard inside the body does not catch the stop" (ticks-spent r)))

;; 3. a body that finishes under its budget answers its value
(ok "under the budget the value comes back"
    (eqv? 5050 (jolt-run-interruptible (jolt-make-interrupt)
                                       (lambda () (let loop ((i 0) (s 0)) (if (> i 100) s (loop (+ i 1) (+ s i)))))
                                       budget)))

;; 4. no budget: the body runs as before (well past any budget above)
(ok "without a budget nothing is bounded"
    (eqv? 3000000 (jolt-run-interruptible (jolt-make-interrupt)
                                          (lambda () (let loop ((i 0)) (if (= i 3000000) i (loop (+ i 1))))))))

;; 5. the token still stops a budgeted body, as an interrupt (no ticks-spent)
(let* ((tok (jolt-make-interrupt))
       (r (guard (e (#t e))
            (jolt-run-interruptible tok (lambda () (jolt-interrupt! tok) (let loop () (loop))) (* 1000 budget)))))
  (ok "a set token stops it first" (and (ex-data-of r) (not (ticks-spent r)))))

;; 6. the handler is given back: a second budgeted run counts from zero
(let ((r (guard (e (#t e))
           (jolt-run-interruptible (jolt-make-interrupt) (lambda () (let loop () (loop))) budget))))
  (ok "a second run spends its own budget" (let ((n (ticks-spent r))) (and n (< n (+ budget 100000))))))

;; 7. a byte budget: a body that allocates in a loop is stopped under a small byte limit,
;; one that allocates nothing with the same limit finishes (and a byte budget alone, no ticks)
(define (bytes-spent e)
  (let ((d (ex-data-of e))) (and d (jolt-get d jolt-kw-bytes-spent #f))))
(define byte-limit 1000000)
(let ((r (guard (e (#t e))
           (jolt-run-interruptible (jolt-make-interrupt)
                                   (lambda () (let loop ((acc '())) (loop (cons (make-vector 100 0) (if (> (length acc) 10) '() acc)))))
                                   #f byte-limit))))
  (ok "an allocating loop is stopped by its byte budget" (bytes-spent r))
  (ok "it allocated its limit and not unboundedly more"
      (let ((n (bytes-spent r))) (and n (>= n byte-limit) (< n (* 100 byte-limit)))))
  (ok "the byte stop is an interrupt and names its budget"
      (and (eq? #t (jolt-get (ex-data-of r) jolt-kw-interrupted #f))
           (eqv? byte-limit (jolt-get (ex-data-of r) jolt-kw-bytes-budget #f))))
  (ok "it is not a tick stop" (not (ticks-spent r))))
(ok "a body that allocates nothing finishes under the same byte limit"
    (eqv? 3000000 (jolt-run-interruptible (jolt-make-interrupt)
                                          (lambda () (let loop ((i 0)) (if (= i 3000000) i (loop (+ i 1)))))
                                          #f byte-limit)))
(ok "bytes 0 is no byte budget"
    (eqv? 7 (jolt-run-interruptible (jolt-make-interrupt)
                                    (lambda () (make-vector 200000 0) 7) #f 0)))
;; both budgets: the tick one still stops a non-allocating loop
(ok "ticks and bytes together: ticks stop a loop that allocates nothing"
    (ticks-spent (guard (e (#t e))
                   (jolt-run-interruptible (jolt-make-interrupt) (lambda () (let loop () (loop))) budget byte-limit))))

(printf "tick-budget: ~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
