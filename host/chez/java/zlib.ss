;; zlib.ss — the zlib entry points java.util.zip calls, and the z_stream they
;; drive. Mechanism only: the JDK 21 classes are translated in zip-*.ss.
;;
;; Resolution. Every jolt binary registers the zlib it links as jolt_z_<name>
;; (host/chez/stub/jolt_zlib.h). Script mode has no jolt launcher, so it falls
;; back to the plain names in the process (Chez's own executable exports its
;; static zlib on macOS and Linux), then to a system libz. A source is used only
;; when it answers for all fourteen names, and on POSIX the system libz gives
;; every name from one declared library: two zlibs have different internal
;; state, so a stream opened by one and run by the other corrupts memory. Plain
;; names are found one at a time in the process, so on Windows a DLL loaded
;; later can still answer for one of them. Resolution
;; happens on first use and never reloads the process handle: a reload
;; re-promotes it above every :jolt/native loaded so far (java/ffi.ss).
;;
;; Memory. zlib keeps next_in and next_out between calls, and the collector may
;; move a bytevector between calls, so the z_stream and both of its buffers
;; live in foreign memory and bytes cross by copy. A buffer larger than
;; zstream-keep-bytes is freed after its call. The caller serializes calls on
;; one stream; zip-*.ss hold a mutex per object.

;; --- constants (zlib.h 1.3.2) -----------------------------------------------
(define z-no-flush 0)
(define z-partial-flush 1)
(define z-sync-flush 2)
(define z-full-flush 3)
(define z-finish 4)
(define z-ok 0)
(define z-stream-end 1)
(define z-need-dict 2)
(define z-stream-error -2)
(define z-data-error -3)
(define z-mem-error -4)
(define z-buf-error -5)
(define z-version-error -6)
(define z-deflated 8)
(define zlib-def-mem-level 8)

;; --- z_stream layout --------------------------------------------------------
;; Field order and C types of z_stream_s. uLong is 64 bits on LP64 and 32 bits
;; on Windows, so the offsets are computed, not written down. Each field aligns
;; to its own size, which holds for every target jolt builds.
;; (zs-layout pointer uInt uLong int) => (values offsets size)
(define (zs-align off size) (* size (quotient (+ off size -1) size)))
(define (zs-layout p u l i)
  ;; next_in avail_in total_in next_out avail_out total_out msg state
  ;; zalloc zfree opaque data_type adler reserved
  (let ((sizes (list p u l p u l p p p p p i l l)))
    (let loop ((ss sizes) (off 0) (acc '()))
      (if (null? ss)
          (values (list->vector (reverse acc)) (zs-align off (apply max sizes)))
          (let ((o (zs-align off (car ss))))
            (loop (cdr ss) (+ o (car ss)) (cons o acc)))))))
(define-values (zs-offsets zs-size)
  (zs-layout (sa-foreign-sizeof 'void*) (sa-foreign-sizeof 'unsigned-int)
             (sa-foreign-sizeof 'unsigned-long) (sa-foreign-sizeof 'int)))
(define zs-next-in 0)
(define zs-avail-in 1)
(define zs-next-out 3)
(define zs-avail-out 4)
(define zs-msg 6)
(define zs-adler 12)
(define (zs-off field) (vector-ref zs-offsets field))

;; --- resolution -------------------------------------------------------------
(define zlib-names
  '#("zlibVersion" "inflateInit2_" "inflate" "inflateEnd" "inflateReset"
     "inflateSetDictionary" "deflateInit2_" "deflate" "deflateEnd" "deflateReset"
     "deflateParams" "deflateSetDictionary" "crc32" "adler32"))
(define zf-version 0)
(define zf-inflate-init 1)
(define zf-inflate 2)
(define zf-inflate-end 3)
(define zf-inflate-reset 4)
(define zf-inflate-dict 5)
(define zf-deflate-init 6)
(define zf-deflate 7)
(define zf-deflate-end 8)
(define zf-deflate-reset 9)
(define zf-deflate-params 10)
(define zf-deflate-dict 11)
(define zf-crc32 12)
(define zf-adler32 13)

;; A foreign-procedure over a runtime entry (a name or an address). Compiled on
;; POSIX, where a petite-only boot cannot eval one; eval'd on Windows, where
;; every build carries the compiler (rt.ss jolt-foreign-proc-safe).
(define-syntax zlib-proc
  (syntax-rules ()
    ((_ entry (arg ...) res)
     (let ((e entry))
       (if (eq? (sa-os-family) 'windows)
           (sa-foreign-procedure-runtime e '(arg ...) 'res #f)
           (sa-foreign-procedure e (arg ...) res))))))

;; The entries LOOKUP gives for every name, in zlib-names order, or #f when
;; one name does not resolve.
(define (zlib-tier lookup)
  (let ((es (vector-map lookup zlib-names)))
    (and (not (memq #f (vector->list es))) es)))

;; PER-NAME holds, for each name in zlib-names order, the (path . address)
;; pairs of the libraries that define it, in declaration order. The addresses
;; from the first library that defines every name, or #f.
(define (zlib-common-definer per-name)
  (let loop ((paths (map car (vector-ref per-name 0))))
    (and (pair? paths)
         (let ((es (vector-map (lambda (defs)
                                 (let ((hit (assoc (car paths) defs)))
                                   (and hit (cdr hit))))
                               per-name)))
           (if (memq #f (vector->list es))
               (loop (cdr paths))
               es)))))

(define zlib-system-loaded? #f)

;; The entries from a system libz, or #f. Script mode only: a built binary
;; resolves jolt_z_<name> first. java/ffi.ss loads after this file, so its
;; procedures are reached at call time, and an unbound one reads as #f.
;; On POSIX this loads libz through jolt.ffi, so the handle stays visible to
;; jolt.ffi/find-symbol for the rest of the process, and takes every name from
;; the first declared library that defines all of them. On Windows the names
;; load-system-library tries come first, then MSYS2's zlib1.dll; the DLLs load
;; one at a time until the process answers for every name, so a DLL without
;; them does not stop the search.
(define (zlib-system-tier)
  (guard (e (#t #f))
    (if (eq? (sa-os-family) 'windows)
        (let ((plain (lambda (name) (and (sa-foreign-entry? name) name))))
          (let try ((dlls '("z.dll" "libz.dll" "zlib1.dll")))
            (or (zlib-tier plain)
                (and (pair? dlls)
                     (begin
                       (guard (e (#t #f)) (sa-load-shared-object (car dlls)))
                       (try (cdr dlls)))))))
        (begin
          (unless zlib-system-loaded?
            (ffi-load-system-library "z")
            (set! zlib-system-loaded? #t))
          (zlib-common-definer (vector-map jolt-ffi-native-resolvers zlib-names))))))

(define (zlib-entries)
  (or (zlib-tier (lambda (name)
                   (let ((private (string-append "jolt_z_" name)))
                     (and (sa-foreign-entry? private) private))))
      (zlib-tier (lambda (name) (and (sa-foreign-entry? name) name)))
      (zlib-system-tier)))

;; The fourteen procedures, in zlib-names order, or #f when no source has all.
(define (zlib-bind-all)
  (let ((es (zlib-entries)))
    (and es
         (vector
          (zlib-proc (vector-ref es zf-version) () uptr)
          (zlib-proc (vector-ref es zf-inflate-init) (uptr int uptr int) int)
          (zlib-proc (vector-ref es zf-inflate) (uptr int) int)
          (zlib-proc (vector-ref es zf-inflate-end) (uptr) int)
          (zlib-proc (vector-ref es zf-inflate-reset) (uptr) int)
          (zlib-proc (vector-ref es zf-inflate-dict) (uptr u8* unsigned-int) int)
          (zlib-proc (vector-ref es zf-deflate-init) (uptr int int int int int uptr int) int)
          (zlib-proc (vector-ref es zf-deflate) (uptr int) int)
          (zlib-proc (vector-ref es zf-deflate-end) (uptr) int)
          (zlib-proc (vector-ref es zf-deflate-reset) (uptr) int)
          (zlib-proc (vector-ref es zf-deflate-params) (uptr int int) int)
          (zlib-proc (vector-ref es zf-deflate-dict) (uptr u8* unsigned-int) int)
          (zlib-proc (vector-ref es zf-crc32) (unsigned-long u8* unsigned-int) unsigned-long)
          (zlib-proc (vector-ref es zf-adler32) (unsigned-long u8* unsigned-int) unsigned-long)))))

(define-process-local zlib-mu (make-mutex))
(define zlib-bound #f)

;; Bound once; after that the vector is read without the lock. A failed bind is
;; not remembered, so a libz loaded later can still be found.
(define (zlib-procs)
  (or zlib-bound
      (jolt-with-mutex zlib-mu
        (or zlib-bound
            (let ((v (zlib-bind-all)))
              (when v (set! zlib-bound v))
              v)))))

(define (zlib-available?) (and (zlib-procs) #t))

(define (zlib-call i)
  (let ((v (zlib-procs)))
    (unless v
      (throw-jvm 'UnsupportedOperationException "java.util.zip: no zlib entry points resolve in this process"))
    (vector-ref v i)))

;; A NUL-terminated C string at ADDR as a Scheme string, or #f for NULL.
(define (zlib-c-string addr)
  (and (not (eqv? addr 0))
       (let loop ((n 0))
         (if (fx= 0 (sa-foreign-ref 'unsigned-8 addr n))
             (let ((bv (make-bytevector n)))
               (sa-foreign-bytes-ref! addr bv n)
               (utf8->string bv))
             (loop (fx+ n 1))))))

(define (zlib-version) (zlib-c-string ((zlib-call zf-version))))

;; --- checksums --------------------------------------------------------------
;; zlib's crc32/adler32 over BV[start, start+count). The bytevector crosses as
;; u8* in a plain call, so it cannot move during the call. A whole bytevector of
;; at most 2^30 bytes (zlib's length is a uInt) goes in one call. Any other range
;; is copied PIECE bytes at a time into one buffer, so a slice of a large array
;; does not double memory.
(define zlib-checksum-piece (* 1024 1024))
(define (zlib-checksum* fn value bv start count piece)
  (let ((f (zlib-call fn)))
    (cond
      ((fx<= count 0) value)
      ((and (fx= start 0) (fx= count (bytevector-length bv)) (fx<= count #x40000000))
       (f value bv count))
      (else
       (let ((buf (make-bytevector (fxmin count piece))))
         (let loop ((value value) (start start) (count count))
           (if (fx<= count 0)
               value
               (let ((n (fxmin count piece)))
                 (bytevector-copy! bv start buf 0 n)
                 (loop (f value buf n) (fx+ start n) (fx- count n))))))))))
(define (zlib-crc32 crc bv start count)
  (zlib-checksum* zf-crc32 crc bv start count zlib-checksum-piece))
(define (zlib-adler32 adler bv start count)
  (zlib-checksum* zf-adler32 adler bv start count zlib-checksum-piece))

;; --- streams ----------------------------------------------------------------
(define-record-type zstream
  (fields (mutable addr)                  ; the z_stream, or 0 once closed
          kind                            ; 'inflate | 'deflate
          (mutable in-buf) (mutable in-cap)
          (mutable out-buf) (mutable out-cap))
  (nongenerative jolt-zstream-v1))

(define (zstream-inflate? zs) (eq? (zstream-kind zs) 'inflate))
(define (zstream-open? zs) (not (eqv? 0 (zstream-addr zs))))

;; The z_stream of an open stream. A closed one throws, as the JDK does after
;; end(); zip-*.ss check first, so this only stops a read at address 0. What is
;; not a zstream at all is the stub an image restore left where an open stream
;; was (state-image.ss image-external?): the address never crossed processes.
(define (zstream-live-addr zs)
  (unless (zstream? zs)
    (jolt-throw (jolt-host-throwable "java.lang.IllegalStateException"
                                     "java.util.zip: the zlib stream was open when its image was written and did not travel; open a new one")))
  (let ((addr (zstream-addr zs)))
    (when (eqv? addr 0)
      (throw-jvm 'NullPointerException "java.util.zip: the zlib stream is closed"))
    addr))

;; Open an inflate or deflate stream: (values zs code message). ZS is #f when
;; zlib refused, and then nothing stays allocated. WBITS is 15 for zlib framing
;; and -15 for raw deflate; LEVEL and STRATEGY are ignored for inflate. Opening
;; drains the guardian first, so reclaimed streams are freed while a program
;; is still making new ones.
(define (zstream-open kind wbits level strategy)
  (zstream-drain!)
  (let* ((version ((zlib-call zf-version)))
         (addr (sa-foreign-alloc zs-size)))
    (sa-foreign-bytes-set! addr (make-bytevector zs-size 0) zs-size)
    (let ((code (guard (e (#t (sa-foreign-free addr) (raise e)))
                  (if (eq? kind 'inflate)
                      ((zlib-call zf-inflate-init) addr wbits version zs-size)
                      ((zlib-call zf-deflate-init) addr level z-deflated wbits
                                                   zlib-def-mem-level strategy version zs-size)))))
      (if (fx= code z-ok)
          (values (make-zstream addr kind 0 0 0 0) code #f)
          (let ((msg (zlib-c-string (sa-foreign-ref 'uptr addr (zs-off zs-msg)))))
            (sa-foreign-free addr)
            (values #f code msg))))))

;; Buffers up to this size stay with the stream between calls; the Inflater and
;; Deflater input window is 64 KiB.
(define zstream-keep-bytes 65536)

;; A buffer of at least N bytes. The new buffer is stored before the old one is
;; freed, so a failed allocation leaves no freed address in the record.
(define (zstream-ensure-in! zs n)
  (when (fx> n (zstream-in-cap zs))
    (let ((old (zstream-in-buf zs)))
      (zstream-in-buf-set! zs (sa-foreign-alloc n))
      (zstream-in-cap-set! zs n)
      (unless (eqv? 0 old) (sa-foreign-free old)))))
(define (zstream-ensure-out! zs n)
  (when (fx> n (zstream-out-cap zs))
    (let ((old (zstream-out-buf zs)))
      (zstream-out-buf-set! zs (sa-foreign-alloc n))
      (zstream-out-cap-set! zs n)
      (unless (eqv? 0 old) (sa-foreign-free old)))))
(define (zstream-trim! zs)
  (when (fx> (zstream-in-cap zs) zstream-keep-bytes)
    (let ((old (zstream-in-buf zs)))
      (zstream-in-buf-set! zs 0) (zstream-in-cap-set! zs 0)
      (sa-foreign-free old)))
  (when (fx> (zstream-out-cap zs) zstream-keep-bytes)
    (let ((old (zstream-out-buf zs)))
      (zstream-out-buf-set! zs 0) (zstream-out-cap-set! zs 0)
      (sa-foreign-free old))))

;; Copy IN into the input buffer, point the z_stream at both buffers, run CALL
;; on the z_stream, and read back how much was used.
(define (zstream-run! zs in out-len call)
  (let ((addr (zstream-live-addr zs))
        (in-len (bytevector-length in)))
    (zstream-ensure-in! zs (fxmax in-len 1))
    (zstream-ensure-out! zs (fxmax out-len 1))
    (sa-foreign-bytes-set! (zstream-in-buf zs) in in-len)
    (sa-foreign-set! 'uptr addr (zs-off zs-next-in) (zstream-in-buf zs))
    (sa-foreign-set! 'unsigned-int addr (zs-off zs-avail-in) in-len)
    (sa-foreign-set! 'uptr addr (zs-off zs-next-out) (zstream-out-buf zs))
    (sa-foreign-set! 'unsigned-int addr (zs-off zs-avail-out) out-len)
    (let* ((code (call addr))
           (consumed (fx- in-len (sa-foreign-ref 'unsigned-int addr (zs-off zs-avail-in))))
           (produced (fx- out-len (sa-foreign-ref 'unsigned-int addr (zs-off zs-avail-out))))
           (out (make-bytevector produced)))
      (sa-foreign-bytes-ref! (zstream-out-buf zs) out produced)
      (zstream-trim! zs)
      (values code consumed produced out))))

;; One inflate or deflate call with flush mode FLUSH over all of IN, with room
;; for OUT-LEN bytes: (values code consumed produced out).
(define (zstream-step! zs flush in out-len)
  (let ((f (zlib-call (if (zstream-inflate? zs) zf-inflate zf-deflate))))
    (zstream-run! zs in out-len (lambda (addr) (f addr flush)))))

;; deflateParams over IN with room for OUT-LEN bytes; it may produce output.
(define (zstream-params! zs level strategy in out-len)
  (let ((f (zlib-call zf-deflate-params)))
    (zstream-run! zs in out-len (lambda (addr) (f addr level strategy)))))

(define (zstream-reset! zs)
  ((zlib-call (if (zstream-inflate? zs) zf-inflate-reset zf-deflate-reset)) (zstream-live-addr zs)))

(define (zstream-set-dictionary! zs dict)
  ((zlib-call (if (zstream-inflate? zs) zf-inflate-dict zf-deflate-dict))
   (zstream-live-addr zs) dict (bytevector-length dict)))

(define (zstream-adler zs) (sa-foreign-ref 'unsigned-long (zstream-live-addr zs) (zs-off zs-adler)))
(define (zstream-message zs) (zlib-c-string (sa-foreign-ref 'uptr (zstream-live-addr zs) (zs-off zs-msg))))

;; End the stream and free its memory. Safe to call again: a closed stream
;; answers z-ok. The memory is freed whatever inflateEnd/deflateEnd returns.
(define (zstream-close! zs)
  (let ((addr (zstream-addr zs)))
    (if (eqv? addr 0)
        z-ok
        (let ((code ((zlib-call (if (zstream-inflate? zs) zf-inflate-end zf-deflate-end)) addr)))
          (zstream-addr-set! zs 0)
          (sa-foreign-free addr)
          (unless (eqv? 0 (zstream-in-buf zs)) (sa-foreign-free (zstream-in-buf zs)))
          (unless (eqv? 0 (zstream-out-buf zs)) (sa-foreign-free (zstream-out-buf zs)))
          (zstream-in-buf-set! zs 0) (zstream-in-cap-set! zs 0)
          (zstream-out-buf-set! zs 0) (zstream-out-cap-set! zs 0)
          code))))

;; --- guardian ---------------------------------------------------------------
;; OWNER is the Java-facing object, and the guardian keeps ZS with MU, the lock
;; the owner holds while it calls zlib. A drain gets (zs . mu) back, never the
;; owner, and closes the stream under MU. An owner can become unreachable while
;; its last call still runs, so without the lock a drain on another thread could
;; free the z_stream under that call. Nothing runs on the collector's thread:
;; streams are freed at the next zstream-open, as jolt.ffi drains its automatic
;; arenas (java/ffi.ss).
(define zstream-guardian (make-guardian))
(define-process-local zstream-guardian-mu (make-mutex))

;; (zstream-guard! owner zs) | (zstream-guard! owner zs mu)
(define (zstream-guard! owner zs . mu)
  (jolt-with-mutex zstream-guardian-mu
    (zstream-guardian owner (cons zs (and (pair? mu) (car mu))))))

(define (zstream-drain!)
  (let ((reclaimed (jolt-with-mutex zstream-guardian-mu
                     (let loop ((acc '()))
                       (let ((rep (zstream-guardian)))
                         (if rep (loop (cons rep acc)) acc))))))
    (for-each (lambda (rep)
                (if (cdr rep)
                    (jolt-with-mutex (cdr rep) (zstream-close! (car rep)))
                    (zstream-close! (car rep))))
              reclaimed)
    (length reclaimed)))
