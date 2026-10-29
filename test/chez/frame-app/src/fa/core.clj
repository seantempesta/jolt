(ns fa.core (:require [fa.lib :as l]))

(defn- top [f]
  (try (vector (f)) nil (catch Throwable e (mapv #(.getClassName ^StackTraceElement %) (.getStackTrace e)))))

(defn -main [& _]
  (l/map-seq 1)
  ;; the error is in the runtime's lazy step (clojure.core/map), not in fa.lib/map-seq
  (println (pr-str (take 1 (top #(doall (map (fn [y] (inc y)) ["a"]))))))
  (println (pr-str (some #{"fa.lib$map_seq"} (top #(doall (map (fn [y] (inc y)) ["a"])))))))
