;; Every change of a var's root inside (jolt.host/on-root-change f thunk) calls f with the var, on the thread that ran it,
;; once the root is written: a def, a def through load-string from a computed string, an intern, an alter-var-root. Seon's
;; REPL binds it around an eval to note what the eval redefined, wherever the definition came from (an agent's computed
;; load-string defines nothing a compiler reference sees). Outside it nothing is called: a process that binds nothing pays
;; one test per root write.
;; Run: target/release/jolt run test/root_change_test.clj
(defn- fail! [& parts]
  (println (apply str "FAIL root-change: " parts))
  (System/exit 1))

(def before 0)
(let [seen (atom [])
      note (fn [v] (swap! seen conj (str (.-ns v) "/" (.sym v))))]
  (jolt.host/on-root-change note
    (fn []
      (load-string (str "(def " "computed-" "name" " 1)"))
      (intern *ns* 'interned 2)
      (alter-var-root #'before inc)))
  (doseq [n ["user/computed-name" "user/interned" "user/before"]]
    (when-not (some #{n} @seen) (fail! n " was not reported: " (pr-str @seen))))
  (reset! seen [])
  (def after-the-hook 3)
  (when (seq @seen) (fail! "reported outside on-root-change: " (pr-str @seen)))
  (println "root-change OK"))
