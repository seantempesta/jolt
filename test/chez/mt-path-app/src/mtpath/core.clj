(ns mtpath.core
  "Prints whether the runtime is on its multi-threaded lazy path once -main has
  done what its argument asks: nothing, register a shutdown hook, or run a
  future (profile-build-smoke.sh)."
  (:gen-class))

(defn -main [& args]
  (case (first args)
    "hook" (.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] nil)))
    "future" @(future 1)
    nil)
  (println (jolt.host/multi-threaded?)))
