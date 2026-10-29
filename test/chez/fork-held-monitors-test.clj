;; A child forked while threads of its parent hold or wait on an object's monitor still enters that
;; monitor, promptly (host/chez/java/concurrency.ss monitor-reset-after-fork!, docs of the
;; runtime's forked child: Seon docs/forks.md). fork(2) copies the forking thread alone, so a
;; monitor another thread held at the fork is held in the child by a thread that does not exist,
;; and the monitor's own mutex and condition carry a waiter that does not exist: the child's
;; `locking` waited on that condition and, where the condition returned at once, spun in
;; monitor-enter-slow!, ~470,000 jolt-lock-wait calls a second and ~1 GB of garbage in 3.5 s (a Seon
;; agent reading a child's two pipes through a poller monitor, 2026-10-29). Here one thread holds the
;; monitor of an object and another waits to enter it, both parked in collect-safe calls (so the
;; fork is allowed) while the main thread forks. The child enters the monitor twice (the second
;; enter is the reentrant and the fast path) and the work done is counted in bytes allocated, never
;; time: a hang (SIGALRM) or a spin is red.
(require '[jolt.scheme :as scm])

(def nap (scm/eval-string "(let ([nap (foreign-procedure __collect_safe \"usleep\" (unsigned) int)]) (lambda (us) (nap us) #t))"))
(def bytes-allocated (scm/eval-string "(lambda () (sa-bytes-allocated))"))

(def fork-beside
  (scm/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)]
           [waitpid (foreign-procedure __collect_safe \"waitpid\" (int u8* int) int)]
           [yield (foreign-procedure \"sched_yield\" () int)]
           [alarm (foreign-procedure \"alarm\" (unsigned) unsigned)])
       (lambda ()
         (alarm 120)
         (let ([pid (let retry ()
                      (disable-interrupts)
                      (mutex-acquire #%$tc-mutex)
                      (let ([pid (if (= (jolt-active-threads) 1) (fork) -2)])
                        (mutex-release #%$tc-mutex)
                        (enable-interrupts)
                        (if (= pid -2) (begin (yield) (retry)) pid)))])
           (if (= pid 0)
               (begin (alarm 30) (jolt-after-fork-child!) -1)
               (let ([st (make-bytevector 4)])
                 (waitpid pid st 0)
                 (alarm 0)
                 (bytevector-s32-native-ref st 0))))))"))

(def lock-object (Object.))
(def held (promise))
(def waiter-in (promise))

;; the holder takes the monitor and sleeps in it; the waiter blocks entering it
(def holder (Thread. #(locking lock-object (deliver held true) (nap 4000000))))
(def waiter (Thread. #(do (deliver waiter-in true) (locking lock-object :entered))))
(.start holder)
@held
(.start waiter)
@waiter-in
(nap 300000)

(def mine (Object.))

;; the child is the fork's thread: a monitor it held at the fork stays held (its exit is its own),
;; and the one held by the parent's other thread is free
(defn child-work []
  (let [b0 (bytes-allocated)
        v (locking lock-object (locking lock-object :in))
        reentered (locking mine (locking mine :again))
        used (- (bytes-allocated) b0)]
    (cond (not= v :in) 3
          (not= reentered :again) 5
          (> used 1000000) 4
          :else 0)))

(def r (fork-beside))
(if (= r -1)
  (System/exit (child-work))
  (let [r2 (locking mine (fork-beside))]
    (if (= r2 -1)
      (System/exit (child-work))
      (let [ok (and (zero? r) (zero? r2))]
        (println (if ok
                   "FORK-HELD-MONITORS-TEST OK"
                   (str "FORK-HELD-MONITORS-TEST FAILED: the child's wait status " r " (forked beside a held monitor), " r2 " (forked inside a locking body); 14 = SIGALRM: it hung on a monitor it inherited held, 4 = it allocated over 1 MB entering one")))
        (flush)
        (System/exit (if ok 0 1))))))
