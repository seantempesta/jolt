;; A host-object method call site caches what the host-type arm resolved.
;;
;; An unhinted (.size q) on a jhost (a queue, an executor, a ByteBuffer…) walked
;; every arm ahead of the host-type arm, each answering 'pass, then hashed the
;; tag and the method name: ~120 ns before .size read one slot, and an argument
;; vector per call. The site (host-static.ss jolt-host-site-call0..3) keeps the
;; tag and procedure the host-type arm resolved, and answers a later call on that
;; tag directly. It fills only when that arm is the one that answered, and a hit
;; also requires the arm list and the method tables to be the ones it filled
;; under, so a new arm or a re-registered method is seen at once.
;;   chez --script test/chez/jhost-method-site-test.ss
(import (chezscheme))
(load "host/chez/rt.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (failure thunk)
  (guard (e (#t (record-method-dispatch (jolt-unwrap-throw e) "getMessage" jolt-nil))) (thunk) #f))

(define lbq (make-abq* "lbq" lbq-unbounded-capacity))
(define abq (make-abq* "abq" 4))
(record-method-dispatch lbq "offer" (jolt-vector 1))
(record-method-dispatch lbq "offer" (jolt-vector 2))

(printf "== a cached call answers what dispatch answers ==\n")
(define s-size (jolt-host-site-make))
(ok "first call (fills)" (= 2 (jolt-host-site-call0 s-size lbq "size")))
(ok "second call (hit)" (= 2 (jolt-host-site-call0 s-size lbq "size")))
(ok "the site filled" (unbox s-size))
(define s-peek (jolt-host-site-make))
(ok "a 0-arg method returning an element" (= 1 (jolt-host-site-call0 s-peek lbq "peek")))
(define s-offer (jolt-host-site-make))
(ok "a 1-arg method" (eq? #t (jolt-host-site-call1 s-offer abq "offer" 9)))
(ok "and again on the cached site" (eq? #t (jolt-host-site-call1 s-offer abq "offer" 8)))
(ok "which really enqueued" (= 2 (record-method-dispatch abq "size" jolt-nil)))

(printf "\n== one site, many receivers ==\n")
(define s-poly (jolt-host-site-make))
(ok "lbq" (= 2 (jolt-host-site-call0 s-poly lbq "size")))
(ok "abq through the same site" (= 2 (jolt-host-site-call0 s-poly abq "size")))
(ok "a string through the same site" (= 3 (jolt-host-site-call0 s-poly "abc" "length")))
(ok "lbq again" (= 2 (jolt-host-site-call0 s-poly lbq "size")))
(ok "nil is the dispatcher's NullPointerException"
    (equal? (failure (lambda () (jolt-host-site-call0 s-poly jolt-nil "size")))
            (failure (lambda () (record-method-dispatch jolt-nil "size" jolt-nil)))))
(define s-arity (jolt-host-site-make))
(ok "a wrong arity is the dispatcher's miss"
    (let ((m (failure (lambda () (jolt-host-site-call1 s-arity lbq "size" 1)))))
      (and m (equal? m (failure (lambda () (record-method-dispatch lbq "size" (jolt-vector 1))))))))
(ok "and leaves the site empty" (not (unbox s-arity)))

(printf "\n== a cached site sees what is registered after it ==\n")
(define s-redef (jolt-host-site-make))
(jolt-host-site-call0 s-redef lbq "isEmpty")
(jolt-host-site-call0 s-redef lbq "isEmpty")
(register-host-methods! "abq" (list (cons "isEmpty" (lambda (self) 'replaced))))
(ok "a re-registered method answers at a filled site"
    (eq? 'replaced (jolt-host-site-call0 s-redef lbq "isEmpty")))
(define s-arm (jolt-host-site-make))
(jolt-host-site-call0 s-arm lbq "size")
(register-method-arm! 3 (lambda (obj name rest)
                          (if (and (eq? obj lbq) (string=? name "size")) 'from-arm 'pass)))
(ok "an arm registered ahead of host-type answers at a filled site"
    (eq? 'from-arm (jolt-host-site-call0 s-arm lbq "size")))

(printf "\n== a hit allocates nothing ==\n")
(define fresh (make-abq* "lbq" lbq-unbounded-capacity))
(define s-alloc (jolt-host-site-make))
(define n 200000)
(define (bytes-per thunk)
  (thunk)
  (let ((b0 (sstats-bytes (statistics))))
    (do ((i 0 (fx+ i 1))) ((fx= i n)) (thunk))
    (quotient (- (sstats-bytes (statistics)) b0) n)))
(define size-bytes (bytes-per (lambda () (jolt-host-site-call0 s-alloc fresh "size"))))
(printf "bytes per cached (.size q): ~a\n" size-bytes)
(ok "a cached 0-arg call allocates nothing beyond the method" (= size-bytes 0))

(printf "\n== a jolt collection's read surface answers what dispatch answers ==\n")
;; A site answers count/size/valAt/get/nth on a jolt map, vector or set itself
;; (host-static.ss native-coll-site?), without the walk to dot-coll-method.
;; Every receiver, method and argument shape must answer what the walk answers,
;; a value or the same exception message.
(define (outcome thunk)
  (guard (e (#t (list 'threw (record-method-dispatch (jolt-unwrap-throw e) "getMessage" jolt-nil))))
    (list 'value (thunk))))
(define (same? a b)
  (and (eq? (car a) (car b))
       (if (eq? (car a) 'value) (jolt-truthy? (jolt= (cadr a) (cadr b))) (equal? (cadr a) (cadr b)))))
(define kw-a (keyword #f "a"))
(define kw-z (keyword #f "z"))
(define big-map (jolt-hash-map-build (let loop ((i 0) (acc '())) (if (= i 40) acc (loop (+ i 1) (cons i (cons (keyword #f (number->string i)) acc)))))))
(define receivers
  (list (cons "array map" (jolt-array-map kw-a 1 (keyword #f "b") 2))
        (cons "hash map" big-map)
        (cons "empty map" (jolt-array-map))
        (cons "vector" (jolt-vector 10 20 30))
        (cons "empty vector" (jolt-vector))
        (cons "map entry" (make-map-entry kw-a 1))
        (cons "set" (jolt-hash-set kw-a 2))
        (cons "empty set" (jolt-hash-set))))
(define calls
  (list (list "count") (list "size")
        (list "valAt" kw-a) (list "valAt" kw-z) (list "valAt" kw-z 'nf) (list "valAt" 1) (list "valAt" 1 'nf)
        (list "get" kw-a) (list "get" kw-z 'nf) (list "get" 0) (list "get" 7) (list "get" 2 'nf)
        (list "nth" 0) (list "nth" 2) (list "nth" 3) (list "nth" -1) (list "nth" 5 'nf) (list "nth" 1 'nf)))
(for-each
  (lambda (r)
    (for-each
      (lambda (c)
        (let* ((name (car c)) (args (cdr c)) (site (jolt-host-site-make))
               (got (outcome (lambda ()
                               (case (length args)
                                 ((0) (jolt-host-site-call0 site (cdr r) name))
                                 ((1) (jolt-host-site-call1 site (cdr r) name (car args)))
                                 (else (jolt-host-site-call2 site (cdr r) name (car args) (cadr args)))))))
               (want (outcome (lambda () (record-method-dispatch (cdr r) name (apply jolt-vector args))))))
          (ok (format "~a: (.~a~{ ~s~}) answers ~s, dispatch ~s" (car r) name args got want) (same? got want))))
      calls))
  receivers)

(printf "\n== ...without the walk ==\n")
;; The walk allocates the argument vector and list; the direct answer allocates nothing.
(define coll-map (jolt-array-map kw-a 1))
(define coll-vec (jolt-vector 1 2 3))
(define s-valat (jolt-host-site-make))
(define s-count (jolt-host-site-make))
(define s-nth (jolt-host-site-make))
(define valat-bytes (bytes-per (lambda () (jolt-host-site-call2 s-valat coll-map "valAt" kw-a jolt-nil))))
(define count-bytes (bytes-per (lambda () (jolt-host-site-call0 s-count coll-vec "count"))))
(define nth-bytes (bytes-per (lambda () (jolt-host-site-call1 s-nth coll-vec "nth" 1))))
(printf "bytes per (.valAt m k nf): ~a, (.count v): ~a, (.nth v i): ~a\n" valat-bytes count-bytes nth-bytes)
(ok "(.valAt m k nf) on a jolt map allocates nothing" (= valat-bytes 0))
(ok "(.count v) on a jolt vector allocates nothing" (= count-bytes 0))
(ok "(.nth v i) on a jolt vector allocates nothing" (= nth-bytes 0))
(register-method-arm! arm-priority-user-override (lambda (obj name rest) (if (and (pvec? obj) (string=? name "count")) 'overridden 'pass)))
(ok "a library override (extend-class!'s tier) still answers first"
    (eq? 'overridden (jolt-host-site-call0 (jolt-host-site-make) coll-vec "count")))

(printf "\n~a checks, ~a failed\n" total fails)
(exit (if (zero? fails) 0 1))
