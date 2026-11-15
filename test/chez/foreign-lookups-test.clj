;; Each foreign-procedure evaluation's lookup is counted by name (our Chez, c/foreign.c S_foreign_lookups;
;; jolt.host/foreign-lookups), so a procedure resolved on every call is named: a root step spent half its
;; samples in dlsym and nothing said who (the owner's item 4, 2026-11-15). A cfn in a reify method resolves
;; per call (jolt.backend/a-cfn-outside-a-def-resolves-once), so 1,000 calls count 1,000 lookups of its name;
;; a defcfn resolves once, however often it is called.
(require '[jolt.ffi :as ffi])

(defn- fail! [& parts] (println (apply str "FAIL foreign-lookups: " parts)) (System/exit 1))

(ffi/defcfn c-getppid "getppid" [] :int)
(def per-call (reify clojure.lang.IFn (invoke [_] ((ffi/cfn "getpgrp" [] :int)))))

(let [n0 (or (get (jolt.host/foreign-lookups) "getpgrp") 0)
      d0 (or (get (jolt.host/foreign-lookups) "getppid") 0)]
  (dotimes [_ 1000] (per-call) (c-getppid))
  (let [m (jolt.host/foreign-lookups)
        n (- (get m "getpgrp" 0) n0)
        d (- (get m "getppid" 0) d0)]
    (when-not (>= n 1000) (fail! "the per-call cfn's lookups are not counted: " n))
    (when-not (<= d 1) (fail! "the defcfn resolved more than once: " d))
    (when-not (re-find #"foreign lookups \(most first\): .*getpgrp" (jolt.host/fiber-dump))
      (fail! "the dump does not name the per-call lookup"))))

(println "FOREIGN-LOOKUPS-TEST OK")
