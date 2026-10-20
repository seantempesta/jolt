;; static-code-share.ss HELPER: a fork()ed child shares the parent's static code.
;;
;; On macOS arm64 fork() copies every resident page of a MAP_JIT mapping into
;; the child, so a child paid its parent's whole code heap up front. With
;; CHEZ_SHARE_STATIC_CODE=1, chez-patches/0007 moves code out of MAP_JIT after
;; a collection into the static generation, and the child shares it
;; copy-on-write. Prints whether the kernel is jolt's patched one, then the
;; child's resident executable KB held privately and shared, as the child
;; itself reads them (HELPER, static-code-share-helper.c).
(load-shared-object "libSystem.dylib")
(load-shared-object (cadr (command-line)))
(define fork (foreign-procedure "fork" () int))
(define waitpid (foreign-procedure "waitpid" (int u8* int) int))
(define pipe (foreign-procedure "pipe" (u8*) int))
(define write-fd (foreign-procedure "write" (int u8* size_t) ssize_t))
(define read-fd (foreign-procedure "read" (int u8* size_t) ssize_t))
(define exit_ (foreign-procedure "_exit" (int) void))
(define usleep (foreign-procedure "usleep" (unsigned-32) int))
(define private-kb (foreign-procedure "static_code_share_private_exec_kb" () long))
(define shared-kb (foreign-procedure "static_code_share_shared_exec_kb" () long))
(collect (collect-maximum-generation) 'static)
;; a context switch after the collection, so the child cannot inherit the
;; collection's code-write view (chez-patches/0006)
(usleep 20000)
(let ((fds (make-bytevector 8 0)))
  (pipe fds)
  (let ((rd (bytevector-s32-native-ref fds 0)) (wr (bytevector-s32-native-ref fds 4))
        (pid (fork)))
    (when (= pid 0)
      (let ((b (make-bytevector 16 0)))
        (bytevector-s64-native-set! b 0 (private-kb))
        (bytevector-s64-native-set! b 8 (shared-kb))
        (write-fd wr b 16)
        (exit_ 0)))
    (let ((b (make-bytevector 16 0)))
      (read-fd rd b 16)
      (waitpid pid (make-bytevector 4 0) 0)
      (printf "~a ~a ~a\n" (foreign-entry? "(cs)lockfree_activation")
              (bytevector-s64-native-ref b 0) (bytevector-s64-native-ref b 8)))))
