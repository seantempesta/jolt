;; A load binds *ns* for its own extent, as Compiler.load does: a loaded file's (ns ...) form
;; writes the load's own binding, never a frame it shares with another fiber. Fibers spawned
;; inside one (binding [*ns* ...]) share that frame by conveyance (as the JVM's Frame.clone
;; shares its TBoxes), so two loads that wrote the current ns through it, each parking part way
;; through its file, set each other's *ns*: a file's later forms interned into the other file's
;; namespace (seon's prove agent: clojure/test/check/random.clj compiled in seon.print-test,
;; "Unable to resolve symbol: rand-long"). On the main carrier, where every wait is a park.
(require '[clojure.core.async :as a] '[clojure.java.io :as io])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))

(def dir (str (System/getProperty "java.io.tmpdir") "/jolt-load-ns-" (System/currentTimeMillis)))
(.mkdirs (io/file dir))
(defn file-for [ns-name v]
  (let [f (str dir "/" ns-name ".clj")]
    ;; its ns form, a park (another load runs meanwhile), then a definition in its own ns
    (spit f (str "(ns " ns-name ")\n"
                 "(clojure.core.async/<!! (clojure.core.async/timeout 20))\n"
                 "(def v " v ")\n"))
    f))

(def result
  (jolt.host/main-carrier-run!
    (fn []
      (binding [*ns* (the-ns 'user)]
        (let [fa (file-for "load-ns.a" 1) fb (file-for "load-ns.b" 2)
              ;; two fibers conveying the one *ns* frame, each loading its file
              ca (a/go (load-file fa) :a)
              cb (a/go (load-file fb) :b)]
          [(a/<!! ca) (a/<!! cb) (str *ns*)
           ;; the owner rule (Var.set's, as jolt-set-var! keeps it): a fiber that inherited
           ;; the binding cannot write the binder's *ns*
           (a/<!! (a/go (try (in-ns 'load-ns.c) :wrote (catch IllegalStateException _ :refused))))
           (str *ns*)])))))

(check-eq "both loads ended" (subvec result 0 2) [:a :b])
(check-eq "an inherited *ns* binding is not the inheritor's to set" (subvec result 3 5) [:refused "user"])
(check-eq "load-ns.a's def is in load-ns.a" (some-> (ns-resolve 'load-ns.a 'v) deref) 1)
(check-eq "load-ns.b's def is in load-ns.b" (some-> (ns-resolve 'load-ns.b 'v) deref) 2)
(check-eq "the spawner's *ns* is its own after both loads" (nth result 2) "user")
(check-eq "neither file defined into the spawner's ns" (contains? (ns-interns 'user) 'v) false)

(if (seq @failures)
  (do (doseq [f @failures] (println "FAIL" f)) (System/exit 1))
  (println "main-carrier-load-ns-test OK"))
