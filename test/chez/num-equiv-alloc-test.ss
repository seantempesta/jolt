;; (== a b) on two plain numbers allocates nothing: clojure.core/== is
;; jolt-num-equiv, called through its var wherever the numeric pass cannot type
;; the operands (a comparator's (== 0 c), datahike's combine-cmp after every
;; field of every datom comparison). As a rest-args procedure it consed its two
;; arguments and walked them twice: 32 bytes a call. Bytes are deterministic
;; where nanoseconds are not, so the invariant is a byte count per call.
;;   chez --script test/chez/num-equiv-alloc-test.ss
(import (chezscheme))
(load "host/chez/rt.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define n 200000)
(define (bytes-per thunk)
  (thunk)
  (let ((b0 (sstats-bytes (statistics))))
    (do ((i 0 (fx+ i 1))) ((fx= i n)) (thunk))
    (quotient (- (sstats-bytes (statistics)) b0) n)))

(define c (- (random 3) 1))
(define x 1.5)
(printf "bytes/call: (== 0 c) ~a, (== 1.5 x) ~a\n"
        (bytes-per (lambda () (jolt-num-equiv 0 c)))
        (bytes-per (lambda () (jolt-num-equiv 1.5 x))))
(ok "(== fixnum fixnum) allocates nothing" (= 0 (bytes-per (lambda () (jolt-num-equiv 0 c)))))
(ok "(== flonum flonum) allocates nothing" (= 0 (bytes-per (lambda () (jolt-num-equiv 1.5 x)))))

;; the semantics the rest-args body has, at every arity
(define (failure thunk) (guard (e (#t 'threw)) (thunk) #f))
(ok "1-arity is true for any value" (eq? #t (jolt-num-equiv "x")))
(ok "(== 3 3.0)" (jolt-num-equiv 3 3.0))
(ok "(== 1 2) is false" (not (jolt-num-equiv 1 2)))
(ok "a long and the double it rounds to are ==, as the call path's fl=? says"
    (jolt-num-equiv 9007199254740993 9007199254740992.0))
(ok "(== 1/2 0.5)" (jolt-num-equiv 1/2 0.5))
(ok "a non-number throws" (eq? 'threw (failure (lambda () (jolt-num-equiv 1 "1")))))
(ok "a non-number first throws" (eq? 'threw (failure (lambda () (jolt-num-equiv "1" 1)))))
(ok "(== 1 1 1)" (jolt-num-equiv 1 1 1))
(ok "(== 1 1 2) is false" (not (jolt-num-equiv 1 1 2)))
(ok "a non-number among three throws" (eq? 'threw (failure (lambda () (jolt-num-equiv 1 1 'a)))))

(printf "num-equiv-alloc: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
