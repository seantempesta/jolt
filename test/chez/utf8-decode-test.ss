;; UTF-8 bytes -> text, against what the JVM's decoder answers
;; (host/chez/java/natives-str.ss utf8-bytes->string, and decode-bytevector in
;; host/chez/java/host-static-classes.ss).
;;   chez --script test/chez/utf8-decode-test.ss
;;
;; Chez's utf8->string and java.nio's decoder agree on every well-formed input.
;; They disagree on malformed input, because they disagree about how many BYTES
;; a bad sequence costs: Java replaces per malformed RUN and decides the run
;; length the way sun.nio.cs.UTF_8's malformedN does, so an overlong lead is
;; rejected alone and the continuation bytes behind it each become a stray of
;; their own. Chez folds the whole sequence into one replacement. They also
;; disagree about a leading BOM, which Chez swallows.
;;
;; jolt used utf8->string for every byte->text seam, so (slurp (io/input-stream
;; f)) answered differently from (slurp f) on the same bytes (jolt-dta.13).
;;
;; What is pinned here:
;;   - the named rows, each one verified against real JVM Clojure
;;   - 2702 random byte strings whose answers came from the JVM
;;     (utf8-decode-oracle.txt — see its header)
;;   - the GUARD's soundness, which is the whole performance argument: when
;;     %utf8-java-plain? says yes, jolt keeps Chez's C decoder, so a false yes
;;     is a silent wrong answer. Every row above is checked that way too, and
;;     so is every sequence of length 1..3 over the bytes at the class
;;     boundaries — 18278 of them, exhaustive where the decoder branches.
;;   - that decode-bytevector, the seam (String. bytes) reaches, uses it

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (cps s) (map char->integer (string->list s)))
(define (bv . bs) (u8-list->bytevector bs))
(define (row name bytes want)
  (let ((got (cps (utf8-bytes->string (u8-list->bytevector bytes)))))
    (set! total (+ total 1))
    (unless (equal? got want)
      (set! fails (+ fails 1))
      (printf "FAIL: ~a\n  bytes ~a\n  got   ~a\n  want  ~a\n" name bytes got want))))

;; --- the named rows ---------------------------------------------------------
;; Every want below is what JVM Clojure 1.12 answered for
;; (String. (byte-array bs) "UTF-8").

(row "ascii"                 '(97 98 99)                  '(97 98 99))
(row "empty"                 '()                          '())
(row "valid 2-byte"          '(194 169)                   '(169))
(row "valid 3-byte"          '(97 226 130 172 98)         '(97 8364 98))
(row "valid 4-byte"          '(240 159 142 137)           '(127881))
(row "CR LF is not touched"  '(97 13 10 98)               '(97 13 10 98))

;; A BOM is content, not a signature: Chez drops it, the JVM hands it back.
(row "leading BOM survives"  '(239 187 191 97)            '(65279 97))
(row "BOM alone"             '(239 187 191)               '(65279))
;; and U+FEFF anywhere else was never in question
(row "BOM mid-string"        '(97 239 187 191 98)         '(97 65279 98))

;; Overlongs. The lead is rejected on its own and each continuation behind it is
;; then a stray, so the replacement count follows the sequence LENGTH -- this is
;; the family Chez collapses to a single replacement.
(row "overlong 2-byte C0 AF" '(97 192 175 98)             '(97 65533 65533 98))
(row "overlong 2-byte C1 BF" '(97 193 191 98)             '(97 65533 65533 98))
(row "overlong 3-byte"       '(97 224 128 175 98)         '(97 65533 65533 65533 98))
(row "overlong 4-byte"       '(97 240 128 128 175 98)     '(97 65533 65533 65533 65533 98))

;; A surrogate is the opposite case: malformedForLength(3), so all three bytes
;; cost ONE replacement.
(row "lone surrogate D800"   '(97 237 160 128 98)         '(97 65533 98))
(row "lone surrogate DFFF"   '(97 237 191 191 98)         '(97 65533 98))

;; Out of range past U+10FFFF, same shape as an overlong: lead alone, then strays.
(row "F4 90 is > 10FFFF"     '(97 244 144 128 128 98)     '(97 65533 65533 65533 65533 98))
(row "F5 lead"               '(97 245 128 128 128 98)     '(97 65533 65533 65533 65533 98))
(row "FF FE"                 '(97 255 254 98)             '(97 65533 65533 98))
(row "bare continuation"     '(97 128 98)                 '(97 65533 98))

;; A valid but INCOMPLETE prefix at end of input underflows and the flush
;; replaces once, however many bytes are left over.
(row "C2 at eof"             '(97 194)                    '(97 65533))
(row "E0 A0 at eof"          '(97 224 160)                '(97 65533))
(row "F0 90 80 at eof"       '(97 240 144 128)            '(97 65533))
;; a lead that is wrong on its own does not get that treatment
(row "C0 at eof"             '(97 192)                    '(97 65533))
;; a valid prefix cut short by a byte that cannot continue it
(row "E2 82 then ascii"      '(97 226 130 98)             '(97 65533 98))

;; U+FFFD spelled out is content like any other character
(row "encoded U+FFFD"        '(97 239 191 189 98)         '(97 65533 98))

;; --- the JVM oracle ---------------------------------------------------------
(define oracle
  (with-input-from-file "test/chez/utf8-decode-oracle.txt"
    (lambda ()
      (let loop ((acc '()))
        (let ((in (read)))
          (if (eof-object? in) (reverse acc) (loop (cons (cons in (read)) acc))))))))

(let ((bad 0) (shown 0))
  (for-each
   (lambda (r)
     (let* ((b (u8-list->bytevector (car r)))
            (got (cps (utf8-bytes->string b))))
       (unless (equal? got (cdr r))
         (set! bad (+ bad 1))
         (when (< shown 6)
           (set! shown (+ shown 1))
           (printf "  oracle mismatch bytes=~a got=~a want=~a\n" (car r) got (cdr r))))))
   oracle)
  (ok (format "~a JVM-pinned rows decode alike" (length oracle)) (= bad 0)))

;; --- the guard --------------------------------------------------------------
;; %utf8-java-plain? is what keeps this off the hot path: it claims the input is
;; one the C decoder cannot get wrong. A false claim is a wrong answer nothing
;; else would catch, so every input in this file is checked against it, and then
;; so is the boundary-exhaustive space.

(define (guard-sound? b)
  (or (not (%utf8-java-plain? b))
      (string=? (utf8->string b) (utf8->string/java b))))

(let ((bad 0))
  (for-each (lambda (r)
              (unless (guard-sound? (u8-list->bytevector (car r)))
                (set! bad (+ bad 1))))
            oracle)
  (ok "the guard never claims a row the C decoder gets wrong" (= bad 0)))

;; every byte at a class boundary in java.nio's decoder, all sequences of
;; length 1..3
(define boundary-bytes
  '(#x00 #x41 #x7F #x80 #x8F #x90 #x9F #xA0 #xBF #xC0 #xC1 #xC2 #xDF
    #xE0 #xE1 #xEC #xED #xEE #xEF #xF0 #xF1 #xF4 #xF5 #xF7 #xF8 #xFF))

(let ((bad 0) (n 0) (first-bad #f))
  (define (visit bs)
    (set! n (+ n 1))
    (let ((b (u8-list->bytevector (reverse bs))))
      (unless (guard-sound? b)
        (set! bad (+ bad 1))
        (unless first-bad (set! first-bad (reverse bs))))))
  (for-each (lambda (a)
              (visit (list a))
              (for-each (lambda (b2)
                          (visit (list b2 a))
                          (for-each (lambda (b3) (visit (list b3 b2 a))) boundary-bytes))
                        boundary-bytes))
            boundary-bytes)
  (when first-bad (printf "  first unsound: ~a\n" first-bad))
  (ok (format "~a boundary sequences: the guard is sound on all" n) (= bad 0)))

;; The slow decoder is the definition, so where the guard says yes the two must
;; be the same answer and not merely both plausible.
(let ((bad 0))
  (for-each (lambda (r)
              (let ((b (u8-list->bytevector (car r))))
                (unless (string=? (utf8-bytes->string b) (utf8->string/java b))
                  (set! bad (+ bad 1)))))
            oracle)
  (ok "the fast path and the model agree on every oracle row" (= bad 0)))

;; Round trip: every code point Chez will hold, encoded and read back.
(let ((bad 0))
  (do ((c 0 (+ c 1))) ((= c #x11000))
    (unless (and (>= c #xD800) (<= c #xDFFF))
      (let* ((s (string (integer->char c)))
             (b (string->utf8 s)))
        ;; a BOM-only string is the one code point Chez's decoder would drop,
        ;; and the model is what has to answer for it
        (unless (string=? (utf8-bytes->string b) s) (set! bad (+ bad 1))))))
  (ok "every code point round trips through the encoder and back" (= bad 0)))

;; --- the seam ---------------------------------------------------------------
;; decode-bytevector is what (String. bytes), slurp of a byte source and the
;; CharsetDecoder all reach; it has to be the same decoder, under every spelling
;; of the charset.
(for-each
 (lambda (name)
   (ok (format "decode-bytevector ~s decodes an overlong like the JVM" name)
       (equal? (cps (decode-bytevector (bv 97 192 175 98) (list name)))
               '(97 65533 65533 98))))
 '("UTF-8" "utf8" "UTF8" "utf-8"))
(ok "decode-bytevector defaults to UTF-8 and keeps the BOM"
    (equal? (cps (decode-bytevector (bv 239 187 191 97) '())) '(65279 97)))
;; the other charsets are not touched by any of this
(ok "latin-1 is still one byte per char"
    (equal? (cps (decode-bytevector (bv 97 192 175 98) '("ISO-8859-1"))) '(97 192 175 98)))
(ok "utf-16be still decodes"
    (equal? (cps (decode-bytevector (bv 0 97 0 98) '("UTF-16BE"))) '(97 98)))

;; (String. bytes offset length cs) over a bare bytevector, which host code
;; hands the constructor (a jolt byte array is the other arm, in the corpus):
;; the slice alone is decoded, and a span outside the bytes is refused.
(ok "a bytevector slice decodes only its bytes"
    (equal? (cps (host-new "String" (bv 104 195 169 108 108 111) 1 3 "UTF-8")) '(233 108)))
(ok "a bytevector slice past the end is refused"
    (guard (e (#t #t)) (host-new "String" (bv 104 195 169) 2 5 "UTF-8") #f))

;; --- the streaming decoder ---------------------------------------------------
;; io-streams.ss open-java-utf8-input-port is what every Reader decodes through.
;; It sees the same bytes in pieces, and the pieces fall wherever the source and
;; the caller happen to put them, so the thing to pin is that it CANNOT answer
;; differently from the whole-buffer decoder above -- which is the half the JVM
;; oracle is pinned against. Anything that survives both is JVM-exact.
;;
;; Two ways to cut the input, and the interesting bugs live at both:
;;   - the SOURCE delivers a few bytes at a time (a pipe, a socket), so a
;;     sequence straddles two refills and has to survive the buffer compaction
;;   - the CALLER asks for a few characters at a time, so the fill has to stop
;;     mid-buffer and resume

;; a binary port that never hands over more than DRIP bytes at once
(define (dripping-port bv drip)
  (let ((pos 0) (n (bytevector-length bv)))
    (make-custom-binary-input-port
     "drip"
     (lambda (dst start count)
       (let ((k (min count drip (- n pos))))
         (bytevector-copy! bv pos dst start k)
         (set! pos (+ pos k))
         k))
     #f #f #f)))

(define (drain bv drip take)
  (let ((p (open-java-utf8-input-port (dripping-port bv drip) "test"))
        (out (open-output-string))
        (scratch (make-string take)))
    (let loop ()
      (let ((got (get-string-n! p scratch 0 take)))
        (if (eof-object? got)
            (begin (close-port p) (get-output-string out))
            (begin (display (substring scratch 0 got) out) (loop)))))))

(define drips '(1 2 3 5 64 70000))
(define takes '(1 2 3 7 1024 70000))

(define (streams-alike? bv)
  (let ((want (utf8-bytes->string bv)))
    (let loop ((ds drips))
      (or (null? ds)
          (and (let inner ((ts takes))
                 (or (null? ts)
                     (and (string=? (drain bv (car ds) (car ts)) want)
                          (inner (cdr ts)))))
               (loop (cdr ds)))))))

;; every named row and every oracle row, through every cut
(let ((bad 0) (first-bad #f))
  (for-each (lambda (r)
              (let ((bv (u8-list->bytevector (car r))))
                (unless (streams-alike? bv)
                  (set! bad (+ bad 1))
                  (unless first-bad (set! first-bad (car r))))))
            oracle)
  (when first-bad (printf "  first streaming mismatch: ~a\n" first-bad))
  (ok (format "~a oracle rows stream the same as they decode whole" (length oracle))
      (= bad 0)))

;; The buffer is 64KB, so a sequence placed either side of that boundary is the
;; case a compaction bug hides in. Walk a tricky sequence across it byte by byte.
(define straddlers
  '((#xE2 #x82 #xAC)               ; a valid 3-byte
    (#xF0 #x9F #x8E #x89)          ; a valid 4-byte
    (#xC0 #xAF)                    ; an overlong, two replacements
    (#xE0 #x80 #xAF)               ; an overlong, three
    (#xED #xA0 #x80)               ; a surrogate, one
    (#xEF #xBB #xBF)))             ; a BOM, which is content here, not a signature

(let ((bad 0) (n 0) (first-bad #f))
  (for-each
   (lambda (seq)
     (do ((pad 65530 (+ pad 1))) ((> pad 65542))
       (let* ((tail '(120 121 122))
              (bv (u8-list->bytevector
                   (append (make-list pad 97) seq tail))))
         (set! n (+ n 1))
         (unless (and (string=? (drain bv 70000 70000) (utf8-bytes->string bv))
                      (string=? (drain bv 4096 4096)   (utf8-bytes->string bv))
                      (string=? (drain bv 3 5)         (utf8-bytes->string bv)))
           (set! bad (+ bad 1))
           (unless first-bad (set! first-bad (list seq pad)))))))
   straddlers)
  (when first-bad (printf "  first straddle mismatch: ~a\n" first-bad))
  (ok (format "~a placements across the 64KB buffer edge decode alike" n) (= bad 0)))

;; and the empty source, which must be eof and not a hang
(ok "an empty source reads as the empty string" (string=? (drain (bytevector) 4 4) ""))

(printf "utf8-decode: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
