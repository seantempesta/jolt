;; jolt.host/main-carrier-run!: once a descriptor is awaited, the loop waits in the io poller's
;; kevent (jolt-main-io-wait), so a fiber another thread queues there must wake that wait, not
;; only the carrier's condition. Unfixed, a go block a future woke never ran: an agent's test fork
;; serving HTTP (ring-chez's acceptor is a future handing each connection to a fiber) never
;; answered, and the judge's run, and every agent's next turn behind it, waited for good (live
;; agents, 2026-10-25). A watchdog thread fails the case instead of letting it hang.
(require '[clojure.core.async :as a])

(def outcome (promise))
(.start (Thread. (fn [] (when (= ::late (deref outcome 10000 ::late))
                          (println "FAIL: the main carrier never ran the fiber a future woke (it waited in kevent)")
                          (flush) (System/exit 1)))))

(def got
  (jolt.host/main-carrier-run!
    (fn []
      (let [ss (java.net.ServerSocket. 0 50 (java.net.InetAddress/getLoopbackAddress))
            c (java.net.Socket. "127.0.0.1" (.getLocalPort ss)) s (.accept ss)
            ;; a read that nothing answers: the loop's wait is now the poller's kevent
            idle (binding [a/*go-backend* :fiber] (a/go (.read (.getInputStream c) (byte-array 1) 0 1)))
            ch (a/chan)
            woken (binding [a/*go-backend* :fiber] (a/go (a/<! ch)))]
        (future (Thread/sleep 20) (a/>!! ch 7))
        (let [r (a/<!! woken)]
          (.close s) (.close c) (.close ss)
          (a/<!! idle)
          r)))))
(deliver outcome got)
(if (= 7 got)
  (do (println "MAIN-CARRIER-WAKE-TEST OK") (flush) (System/exit 0))
  (do (println "FAIL: got" (pr-str got)) (flush) (System/exit 1)))
