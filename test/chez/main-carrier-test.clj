;; The main carrier: a program whose fibers run on its one thread. (jolt.host/main-carrier-run! f)
;; runs f as a fiber on carrier 0 and drives carrier 0 from the calling thread, starting no
;; carrier, timer or poller thread: every wait of f (<!!, >!!, alts!!, a promise's deref, a
;; timeout, a socket's readiness) is a fiber's park, and the loop beneath runs the fibers, the
;; timers and the I/O. Such a program stays one OS thread, so it forks safely while its go
;; blocks and remote channels are mid-traffic; and a wait nothing can ever end raises out of
;; main-carrier-run! instead of hanging.
(require '[clojure.core.async :as a] '[jolt.scheme :as scm] '[jolt.async.remote :as remote])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def fork-alone
  (scm/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)])
       (lambda ()
         (disable-interrupts)
         (mutex-acquire #%$tc-mutex)
         (let ([pid (if (= (#%$top-level-value '$active-threads) 1) (fork) -2)])
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
(def threads
  "This process's OS threads, as the kernel counts them (Mach task_threads; /proc/self/task on
  Linux), not the runtime's Thread objects: a fiber is a virtual thread of its own there, and
  asking ps through clojure.java.shell would start threads of its own. Asked of the parent
  only: a forked child is one thread by fork(2)'s definition, and task_threads called in a
  forked child faulted (invalid memory reference) in about one fork in 400 here."
  (scm/eval-string
    "(if (file-exists? \"/proc/self/task\")
         (lambda () (length (directory-list \"/proc/self/task\")))
         (let ([tt (foreign-procedure \"task_threads\" (unsigned-32 uptr uptr) int)])
           (lambda ()
             (let ([lp (foreign-alloc 8)] [cp (foreign-alloc 4)])
               (tt (foreign-ref 'unsigned-32 (foreign-entry \"mach_task_self_\") 0) lp cp)
               (let ([n (foreign-ref 'unsigned-32 cp 0)]) (foreign-free lp) (foreign-free cp) n)))))"))

(defn in-child
  "The exit status of a forked child that runs (f): 0 when it answers true, 1 else, 2 when it threw."
  [f]
  (let [pid (fork-alone)]
    (cond (zero? pid) (exit-now (try (if (true? (f)) 0 1) (catch Throwable _ 2)))
          (neg? pid) [:refused pid]
          :else (wait-exit pid))))

(def base "the runtime's own threads before the program runs (its shutdown watcher)" (threads))

(def stuck
  (try
    (jolt.host/main-carrier-run!
      (fn []
(check-eq "the mode is on" (jolt.host/main-carrier?) true)

(let [c (a/chan)] (a/go (a/>! c 42)) (check-eq "<!! on the main thread takes what a go block put" (a/<!! c) 42))
(let [c (a/chan)] (a/go (check-eq "a go block takes what >!! put" (a/<! c) 7)) (a/>!! c 7))
(let [p (promise)] (a/go (deliver p :ok)) (check-eq "a promise's deref runs the go block that delivers it" (deref p) :ok))
(check-eq "a timeout closes" (a/<!! (a/timeout 20)) nil)
(let [t (a/timeout 10) [v ch] (a/alts!! [(a/chan) t])] (check-eq "alts!! ends on its timeout" [v (identical? ch t)] [nil true]))
(check-eq "a go loop of 10,000 hops ends" (a/<!! (a/go-loop [i 0] (if (< i 10000) (do (a/<! (a/go i)) (recur (inc i))) i))) 10000)
(check-eq "no carrier, timer or poller thread was started" (threads) base)

;; a remote end to a forked child, both on their main carriers, mid-traffic forks
(let [n 20000
      [fa fb] (remote/socketpair)
      echo (let [pid (fork-alone)]
             (if (zero? pid)
               (exit-now (try (remote/close-fd! fa)
                              (let [e (remote/open fb {:pump :fiber})]
                                (loop [] (when-some [v (a/<!! (:in e))] (a/>!! (:out e) (inc v)) (recur)))
                                (a/close! (:out e))
                                0)
                              (catch Throwable _ 2)))
               pid))
      _ (remote/close-fd! fb)
      e (remote/open fa {:pump :fiber})
      got (atom 0) sum (atom 0)]
  (a/go (dotimes [i n] (a/>! (:out e) i)))
  (let [forks (vec (for [k (range 20)]
                     (do (loop [] (when (< @got (* (inc k) 500)) (swap! sum + (a/<!! (:in e))) (swap! got inc) (recur)))
                         (in-child #(let [c (a/chan)] (a/go (a/>! c 1)) (= 1 (a/<!! c)))))))]
    ;; every value back, then the close: the child exits on it, so nothing it put is still in its buffer
    (loop [] (when (< @got n) (swap! sum + (a/<!! (:in e))) (swap! got inc) (recur)))
    (a/close! (:out e))
    ;; the child's close in turn: its writer flushed, so the waitpid below (a blocking foreign
    ;; call, which stops this one thread's loop) waits on nothing the loop must still run
    (check-eq "the child closes after the parent" (a/<!! (:in e)) nil)
    (check-eq "every value went to the child and back" [@got @sum] [n (reduce + (range 1 (inc n)))])
    (check-eq "20 forks mid-traffic: each child ran a go block and exited 0" (frequencies forks) {0 20})
    (check-eq "the echo child ran to its end" (wait-exit echo) 0)
    (check-eq "the parent started no thread" (threads) base)))

        ;; last: a take no one can ever answer
        (a/<!! (a/chan))
        :returned))
    (catch Throwable t (str (ex-message t) " " t))))
(check-eq "a wait nothing can end raises out of main-carrier-run! instead of hanging"
          (if (and (string? stuck) (re-find #"stuck" stuck)) true stuck) true)

(if (empty? @failures)
  (do (println "MAIN-CARRIER-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
