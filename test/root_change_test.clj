;; Every change of a var's root inside (jolt.host/on-root-change f thunk) calls f with the var once the root is written,
;; on any thread or fiber the thunk started (the binding conveys, as every binding does: a pooled future's, a go block's,
;; an a/thread's): a def, a def through load-string from a computed string, an intern, an alter-var-root. Seon's
;; REPL binds it around an eval to note what the eval redefined, wherever the definition came from (an agent's computed
;; load-string defines nothing a compiler reference sees). Outside it nothing is called: a process that binds nothing pays
;; one test per root write.
;; Run: target/release/jolt run test/root_change_test.clj
(require '[clojure.core.async :as a])
(defn- fail! [& parts]
  (println (apply str "FAIL root-change: " parts))
  (System/exit 1))

(def before 0)
;; the pools warm before the hook, so no thread is made inside it (a new thread would inherit a thread parameter; a warm
;; pool's does not, and missed the root writes a thread parameter carried)
(dotimes [_ 8] @(future 1) (a/<!! (a/thread 1)) (a/<!! (a/go 1)))
(let [seen (atom [])
      note (fn [v] (swap! seen conj (str (.-ns v) "/" (.sym v))))]
  (jolt.host/on-root-change note
    (fn []
      (load-string (str "(def " "computed-" "name" " 1)"))
      (intern *ns* 'interned 2)
      (alter-var-root #'before inc)
      @(future (intern *ns* 'from-future 1))
      (a/<!! (a/thread (intern *ns* 'from-thread 2)))
      (a/<!! (a/go (intern *ns* 'from-go 3)))))
  (doseq [n ["user/computed-name" "user/interned" "user/before" "user/from-future" "user/from-thread" "user/from-go"]]
    (when-not (some #{n} @seen) (fail! n " was not reported: " (pr-str @seen))))
  (reset! seen [])
  (def after-the-hook 3)
  (dotimes [_ 8] @(future (intern *ns* 'after-future 1)) (a/<!! (a/go (intern *ns* 'after-go 2))))
  (when (seq @seen) (fail! "reported outside on-root-change: " (pr-str @seen)))
  (println "root-change OK"))
