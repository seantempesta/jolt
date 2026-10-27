;; test/chez/stack-limit-test.ss — the stack depth limit (rt.ss
;; jolt-install-stack-limit!, the adapter's sa-current-stack-bytes and
;; sa-gc-install-after-rendezvous!). Run:
;;   chez --script test/chez/stack-limit-test.ss   (wired into `make stacklimit`)
;;
;; The pieces a process-level run cannot reach on every host: JOLT_STACK_LIMIT's
;; parser, the default's precedence (RLIMIT_AS, then RLIMIT_DATA, then physical
;; RAM; macOS refuses `ulimit -v` and `ulimit -d`, so only Linux can drive the
;; real thing from a shell), and that the measurement tracks depth. The
;; end-to-end behaviour (the throw, its class, catching it, carrying on after)
;; is test/stack_limit_test.clj, run against the binary by the same gate.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

;; --- 1. JOLT_STACK_LIMIT's spelling ------------------------------------------
(printf "== 1. parsing ==\n")
(for-each
  (lambda (c)
    (let ((got (jolt-stack-limit-parse (car c))))
      (ok (format "1. ~s -> ~s (got ~s)" (car c) (cdr c) got) (equal? got (cdr c)))))
  `(("1048576" . 1048576)
    ("64k" . 65536) ("64K" . 65536)
    ("512m" . 536870912) ("512M" . 536870912)
    ("2g" . 2147483648) ("2G" . 2147483648)
    ;; off, the spellings JOLT_MAX_HEAP takes plus -1
    ("-1" . off) ("0" . off) ("off" . off) ("OFF" . off) ("none" . off) ("None" . off)
    ;; everything else is a configuration error, refused by name at startup
    ("" . #f) ("abc" . #f) ("0m" . #f) ("-2" . #f) ("-1m" . #f)
    ("1.5g" . #f) ("12x" . #f) ("m" . #f) ("2gb" . #f) (" 2g" . #f) ("offf" . #f)))

;; --- 2. the default: 80% of the heap ceiling, else of the memory limit -------
(printf "== 2. default precedence ==\n")
(define gib (* 1024 1024 1024))
(ok "2a. a heap ceiling wins over every rlimit and RAM"
    (= (jolt-stack-limit-default (* 2 gib) (* 8 gib) (* 4 gib) (* 64 gib)) (quotient (* 2 gib 4) 5)))
(ok "2b. no ceiling: RLIMIT_AS wins over RLIMIT_DATA and RAM"
    (= (jolt-stack-limit-default #f (* 8 gib) (* 4 gib) (* 64 gib)) (quotient (* 8 gib 4) 5)))
(ok "2c. RLIMIT_DATA when RLIMIT_AS is unlimited"
    (= (jolt-stack-limit-default #f #f (* 4 gib) (* 64 gib)) (quotient (* 4 gib 4) 5)))
(ok "2d. physical RAM when neither rlimit is set"
    (= (jolt-stack-limit-default #f #f #f (* 64 gib)) (quotient (* 64 gib 4) 5)))
(ok "2e. nothing readable: no limit rather than a guess"
    (not (jolt-stack-limit-default #f #f #f #f)))
(ok "2e'. an exact integer, floored"
    (eqv? (jolt-stack-limit-default 1001 #f #f #f) 800))

;; The default follows the heap ceiling actually in force, whichever of the two
;; is installed first: the stack limit's default is recomputed when the
;; collector policy installs or drops its ceiling.
(define saved-ceiling jolt-heap-ceiling-bytes)
(define saved-installed jolt-gc-policy-installed?)
;; no collector policy (source mode): the base is the ceiling a built program
;; would get, never physical RAM
(set! jolt-gc-policy-installed? #f)
(jolt-stack-limit-install-default!)
(ok (format "2l. no policy installed: 80% of the default ceiling (~a)" (jolt-stack-limit))
    (equal? (jolt-stack-limit)
            (let ((c (heap-ceiling-wanted))) (and c (quotient (* c 4) 5)))))
(set! jolt-gc-policy-installed? #t)
(set! jolt-heap-ceiling-bytes #f)
(jolt-stack-limit-install-default!)
(define no-ceiling-default (jolt-stack-limit))
(set! jolt-heap-ceiling-bytes (* 1 gib))
(jolt-stack-limit-heap-ceiling-changed!)
(ok (format "2i. a ceiling installed after the stack limit is seen (~a)" (jolt-stack-limit))
    (= (jolt-stack-limit) (quotient (* 1 gib 4) 5)))
(set! jolt-heap-ceiling-bytes #f)
(jolt-stack-limit-heap-ceiling-changed!)
(ok "2j. and a dropped one falls back to the memory limit"
    (equal? (jolt-stack-limit) no-ceiling-default))
(jolt-stack-limit-set! (* 64 1024 1024))
(set! jolt-heap-ceiling-bytes (* 1 gib))
(jolt-stack-limit-heap-ceiling-changed!)
(ok "2k. an explicit limit is not replaced by the ceiling's default"
    (= (jolt-stack-limit) (* 64 1024 1024)))
(set! jolt-heap-ceiling-bytes saved-ceiling)
(set! jolt-gc-policy-installed? saved-installed)

;; The real readings on this host: each an exact positive integer or #f.
(define (reading? x) (or (not x) (and (exact? x) (integer? x) (> x 0))))
(ok "2f. RLIMIT_AS reads as bytes or #f" (reading? (jolt-rlimit-bytes 'as)))
(ok "2g. RLIMIT_DATA reads as bytes or #f" (reading? (jolt-rlimit-bytes 'data)))
(ok "2h. the default on this host is a number (physical RAM is readable)"
    (let ((d (jolt-stack-limit-default #f (jolt-rlimit-bytes 'as) (jolt-rlimit-bytes 'data)
                                       (heap-phys-from-sysconf))))
      (and (exact? d) (> d 0))))

;; --- 3. the measurement follows depth -----------------------------------------
(printf "== 3. measurement ==\n")
(define (depth-probe n)
  (if (= n 0)
      (sa-current-stack-bytes)
      (let ((r (depth-probe (- n 1)))) r)))
(let ((shallow (sa-current-stack-bytes))
      (deep (depth-probe 1000000)))
  (ok (format "3a. a 1M-frame recursion measures deeper (~a -> ~a bytes)" shallow deep)
      (and (integer? shallow) (integer? deep) (> (- deep shallow) (* 4 1024 1024)))))
;; the memo: the same depth read twice answers the same, and a read after the
;; recursion returned is back near the start
(let* ((a (depth-probe 200000)) (b (depth-probe 200000)) (c (sa-current-stack-bytes)))
  ;; within a segment: where the splits fall can differ between two runs
  (ok (format "3b. repeatable (~a, ~a)" a b) (< (abs (- a b)) 65536))
  (ok (format "3c. unwound (~a)" c) (< c (quotient a 2))))

;; a one-shot continuation that returned normally is reused by the next
;; call/1cc with a new link and length, so the memo's segment identity alone
;; does not vouch for its total: a memo naming this very segment with another
;; link must be walked past, not added
(let ((m (call/1cc
           (lambda (k)
             (let ((top (#%$current-stack-link)))
               (sa-stack-memo (weak-cons top (vector 1000000000 (weak-cons 'elsewhere #f)
                                                     (#%$continuation-stack-clength top))))
               (sa-current-stack-bytes))))))
  (ok (format "3d. a memo whose segment was relinked is not trusted (~a)" m)
      (< m (* 1024 1024))))

;; --- 4. enforcement, in process -------------------------------------------------
(printf "== 4. enforcement ==\n")
(define (runaway n) (+ 1 (runaway n)))
(define (caught-class thunk)
  (guard (e (#t (let ((v (jolt-unwrap-throw e)))
                    (if (jolt-ex-info-record? v) (jolt-ex-info-record-class-name v) v))))
    (thunk)
    'returned))
(jolt-stack-limit-set! (* 32 1024 1024))
(ok "4a. installed" (= (jolt-stack-limit) (* 32 1024 1024)))
(let ((c (caught-class (lambda () (runaway 1)))))
  (ok (format "4b. runaway recursion raises StackOverflowError (got ~s)" c)
      (equal? c "java.lang.StackOverflowError")))
(let ((c (caught-class (lambda () (runaway 1)))))
  (ok (format "4c. and again after the first unwound (got ~s)" c)
      (equal? c "java.lang.StackOverflowError")))
(ok "4d. ordinary depth still works under the limit" (= 100000 (let d ((n 100000)) (if (= n 0) 0 (+ 1 (d (- n 1)))))))
(jolt-stack-limit-set! #f)
(ok "4e. disabled" (not (jolt-stack-limit)))
(ok "4f. disabled: a recursion past the old limit completes"
    (= 3000000 (let d ((n 3000000)) (if (= n 0) 0 (+ 1 (d (- n 1)))))))

;; --- 5. the check's own failure -----------------------------------------------
;; The hook runs after every collection, on every thread that took part, at a safe point of
;; whatever that thread was doing, and may raise there: that is the StackOverflowError it
;; exists for. A failure of the MEASUREMENT is not that: measuring a stack is a heuristic, and
;; it must never throw into the computation it looks at (a fiber that threw out of the hook
;; answered nothing to the take waiting on it, and Datahike's background gc hung one run in
;; ten on `car: false is not a pair`, 5 of 40 runs with the hook against 0 of 40 with
;; JOLT_STACK_LIMIT=-1). A measurement that fails is "cannot measure" (sa-current-stack-bytes'
;; contract): the check enforces nothing this time, and the failure is kept, with where it
;; happened, for whoever reads (jolt-stack-check-failure, jolt.host/stack-check-failure).
(printf "== 5. the check's own failure ==\n")
(define (search-string s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0)) (and (<= (+ i m) n) (or (string=? (substring s i (+ i m)) sub) (loop (+ i 1)))))))
(jolt-stack-limit-set! (* 1024 1024 1024))
(sa-stack-memo 5)                    ; a memo that is no pair: the measurement cannot read it
(ok "5a. a measurement that fails raises nothing" (begin (jolt-stack-check!) #t))
(let ((f jolt-stack-check-failure))
  (ok (format "5b. and is kept, with where it was raised (~s)" f)
      (and f (eqv? 1 (car f)) (string? (cdr f)) (> (string-length (cdr f)) 0)
           (let ((s (cdr f))) (and (search-string s "car") (search-string s "jolt-stack-check!"))))))
(sa-stack-memo #f)
(jolt-stack-check!)
(ok "5c. a check that measures leaves the record alone" (eqv? 1 (car jolt-stack-check-failure)))
(jolt-stack-limit-set! (* 32 1024 1024))
(ok "5d. and the overflow it exists for still raises"
    (equal? "java.lang.StackOverflowError" (caught-class (lambda () (runaway 1)))))
(jolt-stack-limit-set! #f)

(printf "\n~a checks, ~a failed\n" total fails)
(when (> fails 0) (exit 1))
(printf "stack-limit gate: PASS\n")
