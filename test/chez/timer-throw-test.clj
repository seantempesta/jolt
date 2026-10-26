;; A deadline's thunk that throws on the runtime's timer thread is a thread body's throw: it
;; goes to the default uncaught-exception handler (the JVM's dispatch, jolt-thread-uncaught!),
;; never silence, and the timer thread lives on for every other deadline. Its main-carrier
;; half is main-carrier-timer-throw-test.clj.
(require '[clojure.core.async :as a])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def caught (promise))
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ _ e] (deliver caught (ex-message e)))))

(defn deadline [ms] (+ (/ (System/nanoTime) 1000000) ms))
(jolt.host/timer-at! (deadline 5) (fn [] (throw (ex-info "a deadline's thunk threw" {}))))

(check-eq "the throw reaches the default handler" (deref caught 2000 :never-reached) "a deadline's thunk threw")
(check-eq "and the timer thread fires the next deadline" (a/<!! (a/go (a/<! (a/timeout 20)) :fired)) :fired)

(if (seq @failures)
  (do (doseq [f @failures] (println "FAIL" f)) (System/exit 1))
  (println "timer-throw-test OK"))
