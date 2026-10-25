;; The main carrier waits on a thread the program started (task jolt.async/the-main-carrier-is-
;; stuck-only-when-no-thread-runs): while a future, a Thread or an executor's worker lives it may
;; still wake a fiber, so a wait on it is not stuck. Before, jolt-main-carrier-run! raised stuck
;; when no fiber was queued, no timer set and no descriptor awaited, though the future worked.
;; Once every thread the program started has ended, a wait nothing can answer still raises stuck.
;; One main-carrier-run! per process, with no descriptor awaited (main-carrier-test.clj's own run
;; awaits one, so it never reaches the stuck check this proves).
(def failures (atom []))
(def reached (atom nil))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def stuck
  (try
    (jolt.host/main-carrier-run!
      (fn []
        (check-eq "a promise a working future delivers is waited for, not stuck"
                  (let [p (promise)] (future (Thread/sleep 200) (deliver p :worked)) (deref p)) :worked)
        (check-eq "and one a Thread delivers"
                  (let [p (promise)] (.start (Thread. #(do (Thread/sleep 200) (deliver p :thread)))) (deref p)) :thread)
        ;; last: a wait nothing can answer, every thread the program started having ended
        (reset! reached :last-wait)
        @(promise)
        :returned))
    (catch Throwable t (ex-message t))))
(check-eq "the waits on threads ended, and the program reached its last wait" @reached :last-wait)
(check-eq "a wait nothing can end still raises stuck" (boolean (and (string? stuck) (re-find #"stuck" stuck))) true)

(if (empty? @failures)
  (do (println "MAIN-CARRIER-THREADS-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
