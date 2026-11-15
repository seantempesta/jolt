;; *compiler-options* :no-inline-ns-prefixes: the inline pass splices no callee from those namespaces, so a program whose
;; own vars are redefined in place (a REPL, a load of a changed form) never runs a copy of an old body. Compiled
;; direct-linked, as a build is: a small library fn is spliced into its caller (jolt.host/spliced-fqns names it) unless its
;; namespace's prefix is listed; then the caller calls through the var and sees a redefinition.
;;
;; Run: bin/jolt run test/chez/no-inline-ns-test.clj   (prints NO-INLINE-NS-TEST OK)
(ns no-inline-ns-test)

(defn- fail! [& parts] (println (apply str "FAIL no-inline-ns: " parts)) (System/exit 1))

(defn- spliced? [fqn] (some #{fqn} (jolt.host/spliced-fqns)))

(binding [*ns* *ns* *compiler-options* {:direct-linking true}]
  (eval '(do (ns nin.lib) (defn sq [x] (* x x))))
  (eval '(do (ns nin.app) (defn f [y] (nin.lib/sq y)))))
(when-not (spliced? "nin.lib/sq") (fail! "direct-linked, a small library fn is spliced (else this test shows nothing): " (jolt.host/spliced-fqns)))

(binding [*ns* *ns* *compiler-options* {:direct-linking true :no-inline-ns-prefixes [prog]}]
  (eval '(do (ns prog.lib) (defn sq [x] (* x x))))
  (eval '(do (ns prog.app) (defn f [y] (prog.lib/sq y)))))
(when (spliced? "prog.lib/sq") (fail! "a listed namespace's fn was spliced: " (jolt.host/spliced-fqns)))
(alter-var-root (resolve 'prog.lib/sq) (constantly (fn [x] :redefined)))
(when-not (= :redefined ((resolve 'prog.app/f) 3)) (fail! "the caller of a listed fn did not see its redefinition: " ((resolve 'prog.app/f) 3)))

(println "NO-INLINE-NS-TEST OK")
(System/exit 0)
