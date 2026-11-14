;; A forked child starts with no quiescence handler: jolt.host/on-quiescent!'s handler is the parent's, closed over the
;; parent's socket and state, and a child that inherited it told the parent's peer of the child's quiescence (flow, a
;; Seon agent's handler, 2026-11-14). jolt-after-fork-child! resets it. The parent sets one; the child's dump says none.
;;
;; Run: bin/jolt run test/chez/fork-quiescent-handler-test.clj   (prints FORK-QUIESCENT-HANDLER-TEST OK)
(require '[jolt.scheme :as scm] '[clojure.string :as str])
(def exit-now (scm/eval-string "(foreign-procedure \"_exit\" (int) void)"))
(def wait-status (scm/eval-string "(let ([waitpid (foreign-procedure __collect_safe \"waitpid\" (int uptr int) int)]) (lambda (pid) (let ([b (foreign-alloc 4)]) (waitpid pid b 0) (let ([s (foreign-ref 'int b 0)]) (foreign-free b) (bitwise-arithmetic-shift-right s 8)))))"))
(defn- fail! [& parts] (println (apply str "FAIL fork-quiescent-handler: " parts)) (System/exit 1))
(jolt.host/on-quiescent! (fn [_] nil))
(when-not (str/includes? (jolt.host/fiber-dump) "handler set") (fail! "the parent's handler is not set"))
(let [pid (jolt.host/fork-alone 10000)]
  (cond (zero? pid) (exit-now (if (str/includes? (jolt.host/fiber-dump) "handler none") 0 3))
        (neg? pid) (fail! "the fork was refused: " pid)
        :else (let [st (wait-status pid)]
                (when-not (zero? st) (fail! "the child inherited the parent's quiescence handler (exit " st ")"))
                (println "FORK-QUIESCENT-HANDLER-TEST OK")
                (System/exit 0))))
