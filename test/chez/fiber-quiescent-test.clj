;; jolt.host/on-quiescent! and a remote end's pumps (jolt): a program on its main carrier whose work waits on
;; what nothing will deliver, beside a remote end whose pumps wait on their socket, is quiescent but not
;; deadlocked (the peer may still write). The registered handler hears it once, with the dump, and may end the
;; wait. And a deadlock told to the process is never told to a pump: its link keeps carrying values.
;;
;; Run: bin/jolt run test/chez/fiber-quiescent-test.clj   (prints FIBER-QUIESCENT-TEST OK)
(ns fiber-quiescent-test
  (:require [clojure.core.async :as a]
            [jolt.async.remote :as r]
            [jolt.scheme :as scm]))

(let [res (jolt.host/main-carrier-run!
            (fn []
              (let [[x fd] (r/remote-pair) y (r/open fd)
                    stuck (promise) heard (atom [])
                    _ (jolt.host/on-quiescent! (fn [dump] (swap! heard conj dump) (deliver stuck :ended-by-handler)))
                    v (deref stuck)
                    _ (jolt.host/on-quiescent! nil)
                    ;; a deadlock told now reaches no pump: the link still carries a value both ways
                    _ (scm/eval-string "(jolt-deadlock-tell-fibers! (jolt-deadlock-error))")
                    _ (a/>!! (:out x) :after)
                    got (a/<!! (:in y))]
                (r/close! x) (r/close! y)
                {:v v :heard (count @heard) :dump? (boolean (some #(re-find #"fiber" %) @heard)) :got got})))]
  (if (= {:v :ended-by-handler :heard 1 :dump? true :got :after} res)
    (println "FIBER-QUIESCENT-TEST OK")
    (do (println "FIBER-QUIESCENT-TEST FAILED" res) (System/exit 1)))
  (System/exit 0))
