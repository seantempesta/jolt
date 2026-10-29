;; The main carrier's loop holds carrier 0's mutex around its idle wait. A throw inside that
;; region (here: the deadline read it makes under the mutex, replaced for one call by one that
;; raises) must release the mutex and the lock count, so the error surfaces as itself rather
;; than as a later "mutex is recursively locked" from the next wait. Deterministic: the throw is
;; made by the test, and the check is the counted-lock invariant, no sleep decides anything.
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

;; the next call of jolt-timer-before? raises, then the real one is back
(scm/eval-string
  "(let ([real jolt-timer-before?] [armed? #t])
     (set! jolt-timer-before?
       (lambda (next)
         (if armed?
             (begin (set! armed? #f) (error 'test \"injected throw under carrier 0's mutex\"))
             (real next)))))")

(def thrown
  (try (jolt.host/main-carrier-run! (fn [] (a/<!! (a/timeout 30)) :returned))
       (catch Throwable t (ex-message t))))

(check-eq "the injected throw surfaces as itself, not as a recursive lock"
          (boolean (and (string? thrown) (re-find #"injected throw" thrown))) true)
;; the throw left no counted lock held on this thread (jolt-locks-assert-none! raises when one is)
(check-eq "the loop's region released its counted lock"
          (scm/eval-string "(guard (e (#t #f)) (jolt-locks-assert-none! 'main-carrier-throw-test) #t)") true)
(if (empty? @failures)
  (do (println "MAIN-CARRIER-THROW-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
