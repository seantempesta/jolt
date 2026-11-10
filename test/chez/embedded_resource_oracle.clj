(ns embedded-resource-oracle
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import [java.lang Runtime$Version]))

(def encoded-name "encoded/001 my café;v=1.sql")

(defn url-entry [url]
  (when url
    (let [s (str url)] [(subs s (+ 2 (.indexOf s "!/"))) (slurp url)])))

(defn oracle-data [jar-path]
  (with-open [jar (java.util.jar.JarFile. jar-path)
              loader (java.net.URLClassLoader. (into-array java.net.URL [(.toURL (io/file jar-path))]) nil)]
    (let [manifest (.getManifest jar)
          versions (mapv (fn [version]
                           (with-open [versioned (java.util.jar.JarFile. (io/file jar-path) true 1
                                                                       (Runtime$Version/parse version))]
                             [version
                              (slurp (.getInputStream versioned (.getJarEntry versioned "greeting.txt")))
                              (.getRealName (.getJarEntry versioned "greeting.txt"))
                              (some-> (.getJarEntry versioned "only.txt")
                                      (#(slurp (.getInputStream versioned %))))
                              (some? (.getJarEntry versioned "below.txt"))
                              (some? (.getJarEntry versioned "below7.txt"))
                              (mapv #(.getName %) (iterator-seq (.iterator (.versionedStream versioned))))]))
                         ["8" "9" "17" "22"])]
      {:default-content (slurp (.getInputStream jar (.getJarEntry jar "greeting.txt")))
       :manifest-cached (identical? manifest (.getManifest jar))
       :resources (mapv #(url-entry (.getResource loader %))
                        ["greeting.txt" "only.txt" "below.txt" "below7.txt" "plain.txt" encoded-name "encoded/"])
       :base-version (str (java.util.jar.JarFile/baseVersion))
       :multi-release (.isMultiRelease jar)
       :versioned-names (mapv #(.getName %) (iterator-seq (.iterator (.versionedStream jar))))
       :versions versions
       :manifest [(.getValue (.getMainAttributes manifest) "Multi-Release")
                  (.getValue (.getMainAttributes manifest) "Long-Header")
                  (.getValue (.getMainAttributes manifest) "X-Unicode")
                  (.getValue (.getAttributes manifest "greeting.txt") "X-Entry")]})))

(def audited-classes
  ["java.net.URL" "java.net.URLConnection" "java.net.JarURLConnection"
   "java.util.zip.ZipEntry" "java.util.zip.ZipFile"
   "java.util.jar.JarEntry" "java.util.jar.JarFile"
   "java.util.jar.Manifest" "java.util.jar.Attributes" "java.util.jar.Attributes$Name"
   "java.nio.file.Path" "java.nio.file.FileSystem" "java.nio.file.spi.FileSystemProvider"
   "java.nio.file.FileStore" "java.nio.file.WatchService" "java.nio.file.WatchKey"
   "java.nio.channels.SeekableByteChannel"
   "java.nio.file.attribute.BasicFileAttributeView" "java.nio.file.attribute.BasicFileAttributes"])

(defn canonical [text]
  (str/replace text #"\s+" ""))

(defn parameter-arity [params]
  (if (str/blank? params)
    0
    (loop [chars (seq params) depth 0 arity 1]
      (if-let [[c & more] chars]
        (case c
          \< (recur more (inc depth) arity)
          \> (recur more (dec depth) arity)
          \, (recur more depth (if (zero? depth) (inc arity) arity))
          (recur more depth arity))
        arity))))

(defn javap-rows [class static?]
  (let [result (shell/sh "javap" "-public" class)]
    (when-not (zero? (:exit result))
      (throw (ex-info (str "javap failed for " class) result)))
    (keep (fn [line]
            (let [line (str/trim line)]
              (when (and (str/starts-with? line "public ")
                         (str/includes? line "(")
                         (= static? (str/includes? line " static ")))
                (let [name (-> line (str/split #"\(" 2) first (str/split #"\s+") last)
                      simple (last (str/split class #"\."))
                      constructor? (or (= name class) (str/ends-with? name (str "." simple)))
                      method (if constructor? simple name)]
                  (when (and (not (and (= class "java.net.URL") constructor?))
                             (or constructor? (not (str/includes? method "."))))
                    (let [params (-> line (str/split #"\(" 2) second (str/split #"\)" 2) first)]
                      [class method (canonical params) (parameter-arity params)]))))))
          (str/split-lines (:out result)))))

(defn audit-coverage []
  (let [version (System/getProperty "java.specification.version")
        feature (Integer/parseInt (if (str/starts-with? version "1.") (subs version 2) version))
        all-rows (edn/read-string (slurp "test/chez/embedded-resource-api-coverage.edn"))
        coverage (filter #(<= (or (nth % 5 nil) 21) feature) all-rows)
        covered (set (map #(subvec % 0 4) coverage))
        newer-covered (set (map #(subvec % 0 4) (remove #(<= (or (nth % 5 nil) 21) feature) all-rows)))
        actual (set (concat (mapcat #(javap-rows % false) audited-classes)
                            (javap-rows "java.nio.file.Files" true)
                            [["java.util.jar.JarFile" "baseVersion" "" 0]
                             ["java.util.jar.JarFile" "runtimeVersion" "" 0]]))
        test-source (str/join "\n" (map slurp ["test/chez/embedded-java-app/src/app/core.clj"
                                                "test/chez/embedded-resource-app/src/app/core.clj"]))
        missing-labels (distinct (keep (fn [[_ _ _ _ label]]
                                         (when-not (str/includes? test-source (str "\"" label "\"")) label))
                                       coverage))
        missing (sort (remove (into covered newer-covered) actual))
        extra (when (= feature 21) (sort (remove actual covered)))]
    (when (or (seq missing-labels) (seq missing) (seq extra))
      (let [report {:missing-test-labels (vec missing-labels)
                    :uncovered-signatures (vec missing)
                    :stale-coverage-rows (vec extra)}]
        (binding [*out* *err*] (prn report))
        (throw (ex-info "embedded resource API coverage audit failed" report))))
    (binding [*out* *err*]
      (println "API AUDIT OK" (count covered) "signatures on JDK" feature))))

(defn -main [jar-path]
  (prn (oracle-data jar-path))
  (audit-coverage))
