;; Large byte arrays stay where they are across full collections (#1225).
;;   chez --script test/chez/large-bytes-gc-test.ss
;; A live 8MB byte array cost a full collection ~100us or ~1ms depending only on
;; where its segments came from. Chez pins a huge allocation itself (segment.c
;; S_find_segments: over 128 segments, must_mark) but only when it takes FRESH
;; segments; one satisfied from the free segments of an existing chunk is an
;; ordinary mobile object, and when that chunk is under a quarter used the
;; collector copies rather than marks it -- at every full collection, each copy
;; going back into reused segments. A big array freed earlier is what leaves such
;; a chunk, and whether the request found one moved with the size of the binary,
;; so the gc-arrays bench gate read 1.0x or 4x on the same code. jolt now asks for
;; an immobile bytevector from 64KB up (natives-array.ss na-new-bytes,
;; scheme-adapter-runtime.ss sa-make-large-bytevector).
;;
;; A timing row cannot pin that: under this boot a full collection walks the whole
;; dev image (tens of ms), which hides a 1ms copy. The ADDRESS does pin it, and
;; deterministically: a mobile object is always copied out of generation 0 by its
;; first full collection, so one that has not moved after several is one the
;; collector marks in place, whatever the layout. The control rows say the same
;; probe does see a copy, so a probe that stopped working cannot pass as a fix.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (evv s) (jolt-compile-eval (string-append "(do " s ")") "user"))
(define (ev s) (jolt-final-str (evv s)))
(define (is name s expect) (ok (string-append name " => " expect) (string=? (ev s) expect)))

(define (full!) (collect (collect-maximum-generation)))
(define (addr bv) (#%$fxaddress bv))
;; did BV keep its address across N full collections?
(define (stays? bv n)
  (let ((a (addr bv)))
    (let loop ((i 0))
      (cond ((= i n) #t)
            (else (full!) (and (= a (addr bv)) (loop (+ i 1))))))))
(define (backing x) (jolt-array-vec x))
(define (stays-put name src)
  (let ((a (evv src)))
    (ok (string-append name " is not copied by a full collection")
        (and (bytevector? (backing a)) (stays? (backing a) 3)))))

;; --- the probe sees a copy ---------------------------------------------------
;; A plain 1MB bytevector: multi-segment, but under the 128 segments Chez pins on
;; its own, so it is always mobile -- the case where the copy is guaranteed
;; rather than a matter of which segments the request reused.
(ok "control: a plain 1MB bytevector is copied out of generation 0"
    (not (stays? (make-bytevector (* 1024 1024) 7) 1)))
(ok "control: a small byte array is still an ordinary, mobile bytevector"
    (not (stays? (backing (evv "(byte-array 1024)")) 1)))

;; --- every door to a large byte array ---------------------------------------
(stays-put "(byte-array n)"          "(byte-array (* 8 1024 1024))")
(stays-put "(byte-array n init)"     "(byte-array (* 8 1024 1024) (byte 7))")
(stays-put "(byte-array coll)"       "(byte-array (repeat 70000 1))")
(stays-put "the 64KB threshold"      "(byte-array 65536)")
(stays-put "1MB, under Chez's own pin" "(byte-array (* 1024 1024))")
(stays-put "make-array"              "(make-array Byte/TYPE 70000)")
(stays-put "aclone"                  "(aclone (byte-array 70000))")
(stays-put ".getBytes"               "(.getBytes (apply str (repeat 70000 \\a)))")
(stays-put "a bytevector coerced"    "(byte-array (.getBytes (apply str (repeat 70000 \\a))))")
(stays-put "a byte array coerced"   "(byte-array (byte-array 70000))")

;; --- the bench's shape: a big freed array first, then the live one ------------
;; gc-arrays' order: a 64MB long array freed, then the 8MB byte array, with live
;; data around. Over many collections it must never be copied.
(evv "(def junk (doall (map (fn [i] (str i)) (range 1000))))")
(evv "(let [a (long-array (* 8 1024 1024) 7)] (System/gc) (System/gc) (aget a 0))")
(ok "after a freed 64MB long array: never copied across 50 full collections"
    (stays? (backing (evv "(byte-array (* 8 1024 1024) (byte 7))")) 50))

;; --- the values are the ones asked for ---------------------------------------
(is "zero fill"     "(let [a (byte-array 70000)] [(aget a 0) (aget a 69999) (reduce + a)])" "[0 0 0]")
(is "signed fill"   "(let [a (byte-array 70000 (byte -1))] (System/gc) [(aget a 0) (aget a 69999) (alength a)])" "[-1 -1 70000]")
(is "narrowed fill" "(let [a (byte-array 70000 200)] [(aget a 0) (aget a 69999)])" "[-56 -56]")
(is "written, collected, read back"
    "(let [a (byte-array 70000)] (aset a 69999 (byte 5)) (System/gc) [(aget a 69999) (vec (take 2 (aclone a)))])"
    "[5 [0 0]]")
(is "a clone is a copy"
    "(let [a (byte-array 70000 (byte 3)) b (aclone a)] (aset b 0 (byte 9)) [(aget a 0) (aget b 0)])"
    "[3 9]")
;; dropped ones are reclaimed: immobile is not locked
(ok "dropped large byte arrays are reclaimed"
    (begin
      (full!)
      (let ((before (bytes-allocated)))
        (do ((i 0 (+ i 1))) ((= i 20)) (evv "(byte-array (* 8 1024 1024))"))
        (full!)
        (< (- (bytes-allocated) before) (* 16 1024 1024)))))

(printf "large-bytes-gc-test: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
