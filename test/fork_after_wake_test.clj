;; A fork right after another thread woke this one is made, never refused: the thread that delivered
;; (the timer, a carrier, the poller) is still finishing its turn for a moment after the wake, and
;; jolt.host/fork-alone waits for it to park (Chez's fork_alone, woken as each other thread parks) instead
;; of answering -2. Before, 3 forks in 100 right after a timeout were refused, the agent clone error
;; "cannot fork: n Chez threads active" in production. Each fork's child exits at once.
;; Run: target/release/jolt run test/fork_after_wake_test.clj
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])
(def exit-now (scm/eval-string "(foreign-procedure \"_exit\" (int) void)"))
(def wait-exit (scm/eval-string "(let ([waitpid (foreign-procedure \"waitpid\" (int uptr int) int)]) (lambda (pid) (let ([b (foreign-alloc 4)]) (waitpid pid b 0) (foreign-free b) 0)))"))
(defn try-fork [] (let [pid (jolt.host/fork-alone 10000)] (cond (zero? pid) (exit-now 0) (neg? pid) :refused :else (do (wait-exit pid) :forked))))
(let [c (a/chan)
      _ (a/go (a/<! c))
      n 100
      got (frequencies (for [_ (range n)] (do (a/<!! (a/timeout 2)) (try-fork))))]
  (println (str "fork-after-wake: " (pr-str got)))
  (when-not (= {:forked n} got)
    (println (str "FAIL fork-after-wake: " (:refused got 0) " of " n " forks right after a wake were refused"))
    (System/exit 1))
  (println "fork-after-wake OK"))
