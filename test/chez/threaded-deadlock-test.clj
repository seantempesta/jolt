;; A process run on threads (not a main carrier: a test shard, the forks runner) in which every
;; thread and fiber waits, with nothing that could wake one, tells each waiter so in its own
;; frame: the typed error a main carrier's waiters get (main-carrier-test.clj), {:jolt/deadlock
;; true :parked ...}, which its caller catches and goes on. Before, such a process waited forever
;; (bin/test's shard 3: 61 min; the forks runner's background-gc hang). A wait another thread can
;; still end is never told. The alarm bounds a run that hangs: SIGALRM ends it, red.
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])

((scm/eval-string "(foreign-procedure \"alarm\" (unsigned) unsigned)") 60)

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(defn told [f]
  (try (f) :returned (catch Throwable t (or (:jolt/deadlock (ex-data t)) (str "not told: " t)))))

(check-eq "the main thread's deref of a promise nobody can deliver is told"
          (told #(deref (promise))) true)

(check-eq "and the error names what waited"
          (try (deref (promise)) nil (catch Throwable t (boolean (re-find #"parked|wait" (str (:parked (ex-data t)))))))
          true)

(check-eq "a thread waiting on a monitor in its own while-loop (a re-check for a spurious wake) is told: the raise ends the loop, from inside the wait"
          (told #(let [o (Object.)] (locking o (while true (.wait o)))))
          true)

(check-eq "a take from a go block that died before it put is told (the forks runner's background-gc hang)"
          (told #(let [c (a/chan)] (a/go (throw (ex-info "the go block's throw" {})) (a/>! c 1)) (a/<!! c)))
          true)

(require '[jolt.async.remote :as remote])
(check-eq "after the poller's thread has run (a value through a socket pair), a wait nothing can end is still told: the poller with nothing awaited counts asleep"
          (let [[fa fb] (remote/socketpair) ea (remote/open fa {:pump :fiber}) eb (remote/open fb {:pump :fiber})]
            (a/>!! (:out ea) 7)
            (let [v (a/<!! (:in eb))]
              (remote/close! ea) (remote/close! eb)
              [v (told #(deref (promise)))]))
          [7 true])

;; last: a future's worker idles afterwards in a timed wait, which counts as running
(check-eq "a deref a sleeping thread delivers later is answered, never told"
          (let [p (promise)] (future (Thread/sleep 50) (deliver p :late)) (told #(deref p)))
          :returned)

(if (empty? @failures)
  (do (println "THREADED-DEADLOCK-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
