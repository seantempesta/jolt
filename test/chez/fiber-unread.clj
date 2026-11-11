;; A spawned fiber whose body threw, and whose error nobody ever read (no join, no
;; monitor!), is reported once when it is collected, where every other uncaught throw
;; goes: the default uncaught-exception handler, else stderr. A join that comes later
;; reads it, so a fiber still held is never reported. Each case runs in its own process
;; (fiber-unread.sh) and prints its name, then ok or FAIL with what it saw.
(require '[jolt.fibers :as f])

(def mode (first *command-line-args*))
(def seen (atom []))
(defn handler! []
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ _ ex] (swap! seen conj (ex-message ex))))))

;; Collect until (pred) holds, at most 200 rounds: the report comes after a collection
(defn settle [pred]
  (loop [i 0]
    (cond (pred) true
          (< i 200) (do (System/gc) (Thread/sleep 2) (recur (inc i)))
          :else false)))
(defn collect! [] (settle (constantly false)))

;; N fibers whose bodies throw "<msg> i", none of them kept; returns when all have ended
(def ended (atom 0))
(defn drop-throwing! [msg n]
  (reset! ended 0)
  (dotimes [i n]
    (f/spawn (fn [] (try (throw (ex-info (str msg " " i) {})) (finally (swap! ended inc))))))
  (settle #(= n @ended))
  (Thread/sleep 5))
(defn wait-ended [fib] (settle #(#{:done :dead} (f/state fib))))

;; After a case's own check, a control: a dropped throwing fiber IS reported in this
;; process, so the case's "nothing reported" was not a collection that never ran
(defn control-reported? []
  (let [before (count @seen)]
    (drop-throwing! "control" 1)
    (and (settle #(= (inc before) (count @seen)))
         (= "control 0" (peek @seen)))))

(defn result [ok? what] (println (str "fiber-unread " mode ": " (if ok? "ok" (str "FAIL " what)))))

(case mode
  "unread"
  (do (handler!)
      (drop-throwing! "unread" 1)
      (settle #(seq @seen))
      (collect!)
      (result (= ["unread 0"] @seen) (pr-str @seen)))

  "joined-later"
  (do (handler!)
      (let [fib (f/spawn (fn [] (throw (ex-info "joined later" {}))))]
        (wait-ended fib)
        (collect!)
        (let [before @seen
              joined (try (f/join fib) :returned (catch Exception e (ex-message e)))]
          (collect!)
          (result (and (= [] before @seen) (= "joined later" joined) (control-reported?))
                  (pr-str {:before before :joined joined :seen @seen})))))

  "monitored"
  (do (handler!)
      (let [got (promise)]
        (f/monitor! (f/spawn (fn [] (throw (ex-info "monitored" {})))) #(deliver got (ex-message %)))
        (let [m (deref got 2000 :none)]
          (collect!)
          (result (and (= "monitored" m) (= [] @seen) (control-reported?))
                  (pr-str {:monitor m :seen @seen})))))

  "value"
  (do (handler!)
      (reset! ended 0)
      (f/spawn (fn [] (try 42 (finally (swap! ended inc)))))
      (settle #(= 1 @ended))
      (collect!)
      (result (and (= [] @seen) (control-reported?)) (pr-str @seen)))

  "many"
  (do (handler!)
      (drop-throwing! "many" 1000)
      (settle #(<= 1000 (count @seen)))
      (collect!)
      (result (and (= 1000 (count @seen)) (= (set (map #(str "many " %) (range 1000))) (set @seen)))
              (str (count @seen) " reports, " (count (set @seen)) " distinct")))

  ;; a fiber ended by kill! is not reported: its killer chose its end (Java reports no
  ;; ThreadDeath); one ended by interrupt! is, as an exception leaving a thread's run is
  "killed"
  (do (handler!)
      ((fn [] (let [fib (f/spawn (fn [] @(promise)))]
                (settle #(= :parked (f/state fib)))
                (f/kill! fib (ex-info "killed" {}))
                (wait-ended fib))))
      (Thread/sleep 5)
      (collect!)
      (result (and (= [] @seen) (control-reported?)) (pr-str @seen)))

  "interrupted"
  (do (handler!)
      ((fn [] (let [fib (f/spawn (fn [] @(promise)))]
                (settle #(= :parked (f/state fib)))
                (f/interrupt! fib (ex-info "interrupted" {}))
                (wait-ended fib))))
      (Thread/sleep 5)
      (settle #(seq @seen))
      (collect!)
      (result (= ["interrupted"] @seen) (pr-str @seen)))

  "stderr"
  (do (drop-throwing! "to stderr" 1)
      (collect!)
      (result true ""))

  (result false (str "unknown case " (pr-str mode))))
