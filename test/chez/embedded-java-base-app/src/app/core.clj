(ns app.core)
(defn -main [& _]
  (let [jar (java.util.jar.JarFile. "jolt-embedded-resources.jar")]
    (when-not (= [false "embedded resource ok\n"]
                 [(.isMultiRelease jar)
                  (slurp (.getInputStream jar (.getJarEntry jar "greeting.txt")))])
      (throw (ex-info "non-multi-release archive mismatch" {})))
    (prn :jar-base-ok)))
