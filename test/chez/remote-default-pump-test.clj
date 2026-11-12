;; jolt.async.remote's default pump keeps a forking process one OS thread: ends opened with no
;; :pump inside jolt.host/main-carrier-run! (as a process that forks agents runs) start no thread,
;; so jolt.host/fork-alone, which refuses a process with a second active thread, still forks.
;;
;; Run: bin/jolt run test/chez/remote-default-pump-test.clj   (prints REMOTE-DEFAULT-PUMP-TEST OK)
(ns remote-default-pump-test
  (:require [clojure.core.async :as a]
            [jolt.async.remote :as r]
            [jolt.scheme :as scm]))

(defn- threads [] (scm/eval-string "(length (live-thread-ids))"))

(let [{:keys [before after got]}
      (jolt.host/main-carrier-run!
        (fn []
          (let [before (threads)
                [x fd] (r/remote-pair)
                y (r/open fd)
                _ (a/>!! (:out x) {:n 1})
                got (a/<!! (:in y))
                after (threads)]
            (r/close! x) (r/close! y)
            {:before before :after after :got got})))]
  (if (and (= before after) (= {:n 1} got))
    (println "REMOTE-DEFAULT-PUMP-TEST OK")
    (do (println "REMOTE-DEFAULT-PUMP-TEST FAILED" {:threads-before before :threads-after after :got got})
        (System/exit 1))))
