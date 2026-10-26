;; On the main carrier a deadline's thunk runs in the program's own loop, so a throw from it
;; ends main-carrier-run! with its cause: the program's error, never silence. Its timer-thread
;; half is timer-throw-test.clj.
(require '[clojure.core.async :as a])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(defn deadline [ms] (+ (/ (System/nanoTime) 1000000) ms))
(def result
  (try
    (jolt.host/main-carrier-run!
      (fn []
        (jolt.host/timer-at! (deadline 5) (fn [] (throw (ex-info "a deadline's thunk threw" {}))))
        (a/<!! (a/timeout 50))
        :returned))
    (catch Throwable t (ex-message t))))

(check-eq "the throw ends main-carrier-run! with its cause" result "a deadline's thunk threw")

(if (seq @failures)
  (do (doseq [f @failures] (println "FAIL" f)) (System/exit 1))
  (println "main-carrier-timer-throw-test OK"))
