;; fifo-open-parks' case in a program that requires nothing of core.async: a fiber's open of a FIFO parks on the
;; base runtime alone (a release binary loads core.async's runtime only when a program requires it, and the open's
;; first build reached for core.async's thread there: "variable ... is not bound"). The writer is a plain thread.
(require '[jolt.fibers :as fibers])
(def f (str (System/getProperty "java.io.tmpdir") "/fifo-open-bare-" (System/currentTimeMillis)))
(.waitFor (.start (ProcessBuilder. ^java.util.List ["mkfifo" f])))
(def got
  (jolt.host/main-carrier-run!
    (fn []
      (let [r (fibers/spawn #(try (slurp f) (catch Throwable t (str "threw: " (.getMessage t)))))]
        (Thread/sleep 100)
        (future (spit f "bare"))
        (fibers/join r)))))
(.delete (java.io.File. f))
(shutdown-agents)
(if (= got "bare")
  (println "fifo-open-bare: OK")
  (do (println "FAIL: a fiber's FIFO read with no core.async loaded answered" (pr-str got)) (System/exit 1)))
