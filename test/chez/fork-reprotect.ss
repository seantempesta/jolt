;; fork-reprotect.ss: fork N children from Scheme; each returns into Scheme code
;; and exits 0, while the parent runs a full collection after every fork. Prints
;; the number of children that did not exit 0.
;;
;; On macOS arm64 a collection runs with MAP_JIT code writable, and a child
;; forked by a thread that was descheduled inside one inherits that view instead
;; of the live, re-protected one, so its first instruction back in Scheme faults
;; (chez-patches/0006, build.ss bld-fork-reprotect-defn). Elsewhere the loop is a
;; plain fork smoke. Loaded by test/chez/fork-reprotect-test.sh, both by the
;; kernel alone and inside a jolt binary.
(define (fork-reprotect-run n)
  (unless (foreign-entry? "fork")
    (load-shared-object (if (memq (machine-type) '(arm64osx tarm64osx ta6osx a6osx)) "libSystem.dylib" "libc.so.6")))
  (let ((fork (foreign-procedure "fork" () int))
        (waitpid (foreign-procedure "waitpid" (int u8* int) int))
        (exit_ (foreign-procedure "_exit" (int) void))
        (st (make-bytevector 4 0)))
    (let loop ((i 0) (fails 0))
      (if (= i n)
          fails
          (let ((pid (fork)))
            ;; the child's first work is Scheme code: an arithmetic test, then _exit
            (when (= pid 0) (exit_ (if (= (+ i 1) (+ 1 i)) 0 5)))
            (collect (collect-maximum-generation))
            (waitpid pid st 0)
            (loop (+ i 1) (if (= 0 (bytevector-s32-native-ref st 0)) fails (+ fails 1))))))))
