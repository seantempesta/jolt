;; Each foreign-procedure evaluation's lookup is counted by name (our Chez, c/foreign.c S_foreign_lookups;
;; jolt.host/foreign-lookups), so a procedure resolved on every call is named: a root step spent half its
;; samples in dlsym and nothing said who (the owner's item 4, 2026-11-15). A cfn in a reify method resolves
;; per call (jolt.backend/a-cfn-outside-a-def-resolves-once), so 1,000 calls count 1,000 lookups of its name;
;; a defcfn resolves once, however often it is called (counted at most twice: Jolt's walk and Chez's entry).
(require '[jolt.ffi :as ffi])

(defn- fail! [& parts] (println (apply str "FAIL foreign-lookups: " parts)) (System/exit 1))

(ffi/defcfn c-getppid "getppid" [] :int)
;; called through a function, as a caller that holds the value calls it (a call written in the let that
;; binds a reify can resolve once there)
(defn call-n [f n] (dotimes [_ n] (f)))
(let [;; built in a let, under no def: a cfn inside a def (a reify a def holds included) resolves once
      per-call (reify clojure.lang.IFn (invoke [_] ((ffi/cfn "getpgrp" [] :int))))
      n0 (or (get (jolt.host/foreign-lookups) "getpgrp") 0)
      d0 (or (get (jolt.host/foreign-lookups) "getppid") 0)]
  (call-n per-call 1000) (call-n c-getppid 1000)
  (let [m (jolt.host/foreign-lookups)
        n (- (get m "getpgrp" 0) n0)
        d (- (get m "getppid" 0) d0)]
    (when-not (>= n 1000) (fail! "the per-call cfn's lookups are not counted: " n))
    ;; a defcfn resolves once, at its first call: one resolution touches both counting sites (Jolt's dlsym
    ;; walk and Chez's named entry), so 2, and never more however often it is called
    (when-not (<= d 2) (fail! "the defcfn resolved more than once: " d))
    (when-not (re-find #"foreign lookups \(most first\): .*getpgrp" (jolt.host/fiber-dump))
      (fail! "the dump does not name the per-call lookup"))))

(println "FOREIGN-LOOKUPS-TEST OK")
