;; jolt.host/main-carrier-run! returns, and the process goes on as an ordinary one: a fiber the
;; program left parked runs when its channel is put to, a deadline it left pending fires, a
;; descriptor it left awaited wakes its fiber, and a go block spawned after the return runs on
;; the carrier pool. A process loads on its one thread, forks a child that shares what it loaded,
;; then starts its threads (Seon's root: the fork server is forked at the end of a main-carrier
;; boot). Unfixed, the loop that drove carrier 0 was gone at the return, the pool was "started"
;; at one carrier with no thread, and the timer and poller had none either: every fiber the boot
;; left parked, and every go block after it, waited for good. A watchdog thread fails a case
;; that hangs instead of letting it hang.
(require '[clojure.core.async :as a])

(def at (atom "starting"))
(def outcome (promise))
(.start (Thread. (fn [] (when (= ::late (deref outcome 60000 ::late))
                          (println "FAIL: hung at" @at)
                          (flush) (System/exit 1)))))

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(defn fiber-go [f] (binding [a/*go-backend* :fiber] (a/go (f))))

(def left
  (jolt.host/main-carrier-run!
    (fn []
      (let [ch (a/chan)
            ;; a fiber parked on a channel, a fiber parked on a deadline, a fiber parked on a pipe
            on-chan (fiber-go #(a/<! ch))
            on-timer (fiber-go #(do (a/<! (a/timeout 50)) :fired))
            p (.start (ProcessBuilder. ["sh" "-c" "sleep 0.2; echo ready"]))
            on-fd (fiber-go #(.readLine (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream p)))))]
        {:ch ch :on-chan on-chan :on-timer on-timer :on-fd on-fd :main? (jolt.host/main-carrier?)}))))

(check-eq "the program ran on the main carrier" (:main? left) true)
(check-eq "after the return the process is no main carrier" (jolt.host/main-carrier?) false)

(reset! at "a fiber the program left parked on a channel")
(a/>!! (:ch left) 7)
(check-eq "it runs when its channel is put to" (a/<!! (:on-chan left)) 7)

(reset! at "a deadline the program left pending")
(check-eq "it fires" (a/<!! (:on-timer left)) :fired)

(reset! at "a descriptor the program left awaited")
(check-eq "it wakes its fiber" (a/<!! (:on-fd left)) "ready")

(reset! at "a go block spawned after the return")
(check-eq "it runs on the pool" (a/<!! (fiber-go #(do (a/<! (a/timeout 5)) :ran))) :ran)

(deliver outcome :done)
(if (seq @failures)
  (do (doseq [f @failures] (println "FAIL" f)) (System/exit 1))
  (println "main-carrier-returns-test OK"))
