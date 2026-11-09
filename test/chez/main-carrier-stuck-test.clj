;; jolt.host/main-carrier-run!: the loop raises "stuck" only when nothing can wake its program,
;; read under the lock every waker takes. It used to decide from reads taken at different moments:
;; the queue (empty), then the poller (nothing awaited), then each thread. A thread that woke the
;; program's fiber (queued it) and ended between the first read and the last left the loop seeing
;; no fiber queued and no thread alive; it told only parked fibers (the woken one was queued, not
;; parked), told none, and raised while a fiber sat in its queue. A Seon root died so at boot under
;; load: its boot fiber derefs the futures clojure.java.shell starts (2026-11-09). Here the window
;; is held open deterministically: the loop's own main-io-wait hook is called inside it, and lets
;; the waking thread deliver and end there. And a program that truly cannot go on (its one wait
;; masked, so no waiter can be told) gets the stuck error with every wait named in it, as the
;; deadlock error has them. No watchdog thread: one that may run would itself keep the loop from
;; deciding (the Makefile's alarm ends a hang).
(require '[jolt.scheme :as scm] '[jolt.fibers :as fibers])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def fork-alone
  (scm/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)])
       (lambda ()
         (disable-interrupts)
         (mutex-acquire #%$tc-mutex)
         (let ([pid (if (= (jolt-active-threads) 1) (fork) -2)])
           (mutex-release #%$tc-mutex)
           (enable-interrupts)
           (when (= pid 0) (jolt-after-fork-child!))
           pid)))"))
(def wait-exit
  (scm/eval-string
    "(let ([waitpid (foreign-procedure \"waitpid\" (int uptr int) int)])
       (lambda (pid)
         (let ([b (foreign-alloc 4)])
           (waitpid pid b 0)
           (let ([s (foreign-ref 'int b 0)]) (foreign-free b) (if (= (logand s 127) 0) (ash s -8) 99)))))"))
(def exit-now (scm/eval-string "(let ([e (foreign-procedure \"_exit\" (int) void)]) (lambda (n) (e n)))"))

;; 1. In a child (a main carrier that raised is not left, so it is the child's only one): a wait
;; no one can be told of (masked) raises stuck, with the program's wait named in :parked.
(check-eq "a masked wait nothing can end raises stuck, naming its wait"
          (let [pid (fork-alone)]
            (if (zero? pid)
              (exit-now
                (try (jolt.host/main-carrier-run! (fn [] (fibers/masked #(deref (promise)))))
                     1
                     (catch Throwable t
                       (let [d (ex-data t)]
                         (cond (not (re-find #"stuck" (str (ex-message t)))) 2
                               (not (:jolt/stuck d)) 3
                               (not (re-find #"parked" (str (:parked d)))) 4
                               :else 0)))))
              (wait-exit pid)))
          0)

;; 2. A thread queues the fiber and ends inside the window between the loop's queue read and its
;; thread scan: the fiber runs, and the program answers.
(def latch (promise))
(def p (promise))
(def t (doto (Thread. (fn [] @latch (deliver p 42)) "waker") (.setDaemon true)))
(def staged (atom false))
(jolt.host/main-io-wait-set!
  (fn [ms]
    ;; the loop found nothing queued and asks the poller: the waker queues the fiber and ends now
    (when (compare-and-set! staged false true)
      (deliver latch true)
      (.join ^Thread t))
    false)
  (fn [] nil)
  (fn [] nil))
(check-eq "a thread that queues the fiber and ends between the loop's reads: the fiber runs"
          (try (jolt.host/main-carrier-run! (fn [] (.start ^Thread t) @p))
               (catch Throwable e (str "THREW: " (ex-message e))))
          42)
(check-eq "the race was staged inside the window" @staged true)

(if (empty? @failures)
  (do (println "MAIN-CARRIER-STUCK-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
