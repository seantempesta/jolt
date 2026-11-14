;; A forked child never runs a fiber its parent had parked (tests, 2026-11-14, the two-test-runs hang reading: a run's
;; fiber parked at a sibling's fork, woken in the child by the child's own put, forked its own child from there).
;; jolt-after-fork-child! drops the parent's READY fibers (seon.lib-test/a-forked-child-is-reset-before-anything-can-
;; preempt-it); a PARKED one stays reachable from what it waits on, so a put in the child must not resume it. The
;; parent parks a fiber on a channel and forks; the child puts on that channel and exits 3 if the parked fiber ran.
;;
;; Run: bin/jolt run test/chez/fork-parked-fiber-test.clj   (prints FORK-PARKED-FIBER-TEST OK)
(require '[clojure.core.async :as a] '[jolt.scheme :as scm])
(def exit-now (scm/eval-string "(foreign-procedure \"_exit\" (int) void)"))
(def wait-status (scm/eval-string "(let ([waitpid (foreign-procedure __collect_safe \"waitpid\" (int uptr int) int)]) (lambda (pid) (let ([b (foreign-alloc 4)]) (waitpid pid b 0) (let ([s (foreign-ref 'int b 0)]) (foreign-free b) (bitwise-arithmetic-shift-right s 8)))))"))
(defn- fail! [& parts] (println (apply str "FAIL fork-parked-fiber: " parts)) (System/exit 1))
(def ran (atom false))
(def r
  (jolt.host/main-carrier-run!
    (fn []
      (let [ch (a/chan)
            parked (a/go (a/<! ch) (reset! ran true))]
        (Thread/sleep 50)   ; the go block is parked on ch
        (let [pid (jolt.host/fork-alone 5000)]
          (cond (zero? pid) (do (a/offer! ch :in-the-child)   ; a put in the child: whoever takes it runs here
                                (Thread/sleep 100)
                                (exit-now (if @ran 3 0)))
                (neg? pid) [:refused pid]
                :else (let [st (wait-status pid)] (a/close! ch) [:child-exit st])))))))
(cond (= r [:child-exit 0]) (do (println "FORK-PARKED-FIBER-TEST OK") (System/exit 0))
      (= r [:child-exit 3]) (fail! "the child ran a fiber its parent had parked")
      :else (fail! "unexpected: " (pr-str r)))
