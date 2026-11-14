;; A fork beside a thread on its way into a collect-safe call is made, never waited out for good: Chez's
;; fork_alone waits for the other threads to park, and a thread entering C only marks itself in its own word
;; (Sdeactivate_thread), which woke no fork that had already looked and gone to wait. The fork waited forever
;; beside a thread blocked in read(2) (a Seon test child, 2026-11-14: its main thread in S_fork_alone >
;; pthread_cond_wait in all 801 samples). Each of 100 rounds starts a raw thread that blocks in a collect-safe read of a
;; pipe nobody writes, and forks at once; the child exits. The bound is a thread asleep in a collect-safe
;; usleep, then jolt.host/fork-alone-cancel!: a thread parked in a Scheme sleep would wake the fork as it parks
;; and hide the defect. Estimate: 100 forks of this process and their reaps, well under a second.
;; Run: target/release/jolt run test/fork_beside_native_entry_test.clj
(require '[jolt.scheme :as scm])
(def exit-now (scm/eval-string "(foreign-procedure \"_exit\" (int) void)"))
(def wait-exit (scm/eval-string "(let ([waitpid (foreign-procedure \"waitpid\" (int uptr int) int)]) (lambda (pid) (let ([b (foreign-alloc 4)]) (waitpid pid b 0) (foreign-free b) 0)))"))
(def start-reader
  (scm/eval-string "(let ([pipe (foreign-procedure \"pipe\" (u8*) int)]
                          [rd (foreign-procedure __collect_safe \"read\" (int uptr size_t) ssize_t)])
                      (lambda ()
                        (let ([fds (make-bytevector 8 0)])
                          (pipe fds)
                          (let ([r (bytevector-s32-native-ref fds 0)] [buf (foreign-alloc 16)])
                            (fork-thread (lambda () (rd r buf 16)))))))"))
(def start-bound
  (scm/eval-string "(let ([usleep (foreign-procedure __collect_safe \"usleep\" (unsigned-32) int)]
                          [cancel (foreign-procedure \"(cs)fork_alone_cancel\" () void)])
                      (lambda () (fork-thread (lambda () (let loop ([n 10]) (unless (= n 0) (usleep 1000000) (loop (- n 1)))) (cancel)))))"))
(start-bound)
(let [n 100
      ;; the rounds stop at the first refusal: the bound is spent
      got (loop [i 0]
            (if (= i n)
              {:forked n}
              (do (start-reader)
                  (let [pid (jolt.host/fork-alone)]
                    (cond (zero? pid) (exit-now 0)
                          (neg? pid) {:forked i :refused 1}
                          :else (do (wait-exit pid) (recur (inc i))))))))]
  (println (str "fork-beside-native-entry: " (pr-str got)))
  (when-not (= {:forked n} got)
    (println (str "FAIL fork-beside-native-entry: round " (inc (:forked got)) " of " n " waited until its bound beside a thread entering a collect-safe read"))
    (System/exit 1))
  (println "fork-beside-native-entry OK")
  (System/exit 0))
