;; jolt.host/main-carrier-run!: a fiber another thread queues runs whatever the loop waits in.
;; Once a descriptor is awaited the loop waits in the io poller's kevent (jolt-main-io-wait), and
;; with a timer pending it waits up to the timer; either wait must end when another thread
;; queues a fiber, or that fiber does not run. Unfixed, a go block a future woke never ran: an
;; agent's test fork serving HTTP (ring-chez's acceptor is a future handing each connection to a
;; fiber) never answered, and the judge's run, and every agent's next turn behind it, waited for
;; good (live agents, 2026-10-25). The wake is made after the carrier's mutex is released: the
;; park path takes that mutex under the poller's lock (the stress case). A watchdog thread fails a
;; case that hangs instead of letting it hang.
(require '[clojure.core.async :as a])

(def at (atom "starting"))
(def outcome (promise))
(.start (Thread. (fn [] (when (= ::late (deref outcome 60000 ::late))
                          (println "FAIL: hung at" @at "(a lost wake, or a deadlock)")
                          (flush) (System/exit 1)))))

(defn fiber-go [f] (binding [a/*go-backend* :fiber] (a/go (f))))

(def got
  (jolt.host/main-carrier-run!
    (fn []
      (let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
            c (java.net.Socket. "127.0.0.1" (.getLocalPort ss)) s (.accept ss)
            in (.getInputStream c) out (.getOutputStream s)
            r1 (do (reset! at "a future wakes a go block while the loop waits in kevent")
                   (let [idle (fiber-go #(.read in (byte-array 1) 0 1))
                         ch (a/chan) woken (fiber-go #(a/<! ch))]
                     (future (Thread/sleep 20) (a/>!! ch 7))
                     (let [r (a/<!! woken)] (.write out (byte-array [1]) 0 1) (a/<!! idle) r)))
            ;; nothing awaited, a timer 30 s out: the go block runs when the future wakes it,
            ;; not when the timer ends the loop's wait (the loop runs its queue before its timers,
            ;; so only when it ran tells the two apart: 15 s is a margin no load reaches against a
            ;; 20 ms wake)
            r2 (do (reset! at "a future wakes a go block while the loop waits up to a timer")
                   (let [ch (a/chan) woken (fiber-go #(a/<! ch)) t (a/timeout 30000) t0 (System/nanoTime)]
                     (future (Thread/sleep 20) (a/>!! ch 8))
                     (let [v (a/<!! woken) ms (/ (- (System/nanoTime) t0) 1e6)]
                       (if (< ms 15000) v [:ran-at-the-timer ms]))))
            ;; the lock order: futures resume fibers (the carrier's mutex, then the wake) while
            ;; the main fiber parks on a socket again and again (the poller's lock, then the mutex)
            r3 (do (reset! at "eight futures resume fibers while the main fiber parks on a socket")
                   (let [n 200 ch (a/chan 16)
                         sum (fiber-go #(loop [k 0 acc 0] (if (= k (* 8 n)) acc (recur (inc k) (+ acc (a/<! ch))))))
                         fs (doall (for [_ (range 8)] (future (dotimes [_ n] (a/>!! ch 1)))))]
                     (dotimes [_ 200]
                       (let [rd (fiber-go #(.read in (byte-array 1) 0 1))]
                         (.write out (byte-array [1]) 0 1)
                         (a/<!! rd)))
                     (run! deref fs)
                     (a/<!! sum)))]
        (.close s) (.close c) (.close ss)
        [r1 r2 r3]))))
(deliver outcome got)
(if (= [7 8 1600] got)
  (do (println "MAIN-CARRIER-WAKE-TEST OK") (flush) (System/exit 0))
  (do (println "FAIL: got" (pr-str got)) (flush) (System/exit 1)))
