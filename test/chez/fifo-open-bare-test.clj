;; fifo-open-parks' case in a program that has loaded nothing else: no core.async, and no process started (the
;; gate makes the FIFO and passes its path in FIFO; this test deletes it), so neither core.async's runtime nor
;; process.ss, which a release binary loads lazily, is there unless the open asks for it (it did not: "variable
;; ... is not bound" under jolt -e). The writer is a plain thread.
(require '[jolt.fibers :as fibers])
(def f (System/getenv "FIFO"))
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
