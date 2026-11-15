;; A cfn outside a def (a reify, deftype or defrecord method, an extend-type impl in a bare top-level form) resolves its
;; symbol once a process: the site keeps the procedure in a box of the form's constant pool. It resolved on every call
;; (two dlsym walks a call: 15 us at Seon's root, task jolt.backend/a-cfn-outside-a-def-resolves-once).
;;
;; One resolution counts at most twice (Jolt's dlsym walk and Chez's entry, as a defcfn's).
;;
;; Run: bin/jolt run test/chez/cfn-outside-def-test.clj   (prints CFN-OUTSIDE-DEF-TEST OK)
(ns cfn-outside-def-test (:require [jolt.ffi]))

(defn- fail! [& parts] (println (apply str "FAIL cfn-outside-def: " parts)) (System/exit 1))
(defn- lookups [nm] (get (jolt.host/foreign-lookups) nm 0))

(let [r (reify clojure.lang.IFn (invoke [_] ((jolt.ffi/cfn "getpgrp" [] :int))))
      n0 (lookups "getpgrp")]
  (dotimes [_ 1000] (r))
  (let [n (- (lookups "getpgrp") n0)]
    (when-not (<= n 2) (fail! "a cfn in a reify outside a def resolved " n " times in 1000 calls"))))

(deftype CfnT [] clojure.lang.IFn (invoke [_] ((jolt.ffi/cfn "getppid" [] :int))))
(let [t (CfnT.) n0 (lookups "getppid")]
  (dotimes [_ 1000] (t))
  (let [n (- (lookups "getppid") n0)]
    (when-not (<= n 2) (fail! "a cfn in a deftype method resolved " n " times in 1000 calls"))))

(println "CFN-OUTSIDE-DEF-TEST OK")
(System/exit 0)
