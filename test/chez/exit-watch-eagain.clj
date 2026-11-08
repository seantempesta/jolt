;; test/chez/exit-watch-eagain.clj -- exit-watch asks again when the kernel answers EAGAIN.
;; Run through jolt (wired into host/chez/smoke.sh).
;;
;; WHY THIS EXISTS. kevent(EVFILT_PROC, NOTE_EXIT) on a just-forked child under load 20-40
;; answered EAGAIN (errno 35), and exit-watch threw "kevent refused the watch" for it, failing
;; one agent in fifty. EAGAIN is transient; only the other errnos (ESRCH aside) are refusals.
;; The retry loop is exercised with the kernel's answers injected; the real watch is exercised
;; on a live child and on a pid that is gone.
(require '[jolt.io-poller])
(require '[clojure.core.async :as a])

(def ask   @#'jolt.io-poller/ask-while-eagain)
(def bound @#'jolt.io-poller/EXIT-WATCH-ATTEMPTS)
(def EAGAIN (if (= "Mac OS X" (System/getProperty "os.name")) 35 11))

(defn answers [& rs] (let [q (atom rs) n (atom 0)] [n #(do (swap! n inc) (let [r (first @q)] (swap! q rest) r))]))

(def fails (atom []))
(defn check [label ok] (when-not ok (swap! fails conj label)))

;; EAGAIN twice, then success: asked three times, the success returned
(let [[n f] (answers (- EAGAIN) (- EAGAIN) 7)]
  (check "success after EAGAIN" (and (= 7 (ask f)) (= 3 @n))))
;; ESRCH (3) is not retried
(let [[n f] (answers -3 7)]
  (check "other errno not retried" (and (= -3 (ask f)) (= 1 @n))))
;; EAGAIN forever: asked exactly the bound, the last refusal returned
(let [n (atom 0)]
  (check "bounded" (and (= (- EAGAIN) (ask #(do (swap! n inc) (- EAGAIN)))) (= bound @n))))

;; the real watch: a live child's exit is answered, a reaped pid is :gone
(let [exit-watch @#'jolt.io-poller/exit-watch
      p (.start (ProcessBuilder. ["sh" "-c" "sleep 1"]))
      pid (.pid p)
      ch (jolt.io-poller/exit-chan pid)]
  (check "live child's exit" (= pid (a/<!! ch)))
  (.waitFor p)
  (check "reaped pid is gone" (= :gone (exit-watch pid))))

(if (empty? @fails)
  (println "EXIT-WATCH-EAGAIN OK")
  (do (println "EXIT-WATCH-EAGAIN FAILED" (pr-str @fails)) (System/exit 1)))
