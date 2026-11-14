;; Task jolt.host/a-blocking-call-on-a-main-carrier-parks (Seon). On a main carrier every fiber shares one OS thread, so
;; an open(2) of a FIFO nobody has opened the other end of, run there in C, holds the carrier: no other fiber runs,
;; quiescence is never told, and the wait is invisible to a fiber dump (context's refine, 2026-11-13: an open that blocked
;; 80 minutes). An open of a path that may block parks its fiber instead, as a socket or a child's pipe does, so the
;; carrier goes on. Both directions: a read open waits for a writer, a write open for a reader. The other end is a child
;; process that opens it after a pause, so the wait is real; each side must see the other fiber tick meanwhile, and get
;; what the other end sent. A regular file opens on the ordinary path (the last check).
(require '[clojure.java.shell :as shell] '[jolt.fibers :as fibers])

(def failures (atom []))
(defn check [what ok detail]
  (when-not ok (swap! failures conj (str what ": " (pr-str detail)))))

(def dir (str (System/getProperty "java.io.tmpdir") "/fifo-open-parks-" (System/currentTimeMillis)))
(.mkdirs (java.io.File. dir))
(defn fifo [n] (let [p (str dir "/" n)] (shell/sh "mkfifo" p) p))

(defn ticks-during
  "Runs open-and-use on this carrier while a sibling fiber counts its own turns (a park of 5 ms each); answers
  [what open-and-use answered, the turns the sibling took while it ran]."
  [open-and-use]
  (let [n (atom 0) stop (atom false)
        ticker (fibers/spawn #(loop [] (when-not @stop (swap! n inc) (Thread/sleep 5) (recur))))
        _ (Thread/sleep 20)
        before @n
        got (open-and-use)
        during (- @n before)]
    (reset! stop true) (fibers/join ticker)
    [got during]))

(def on-carrier
  (jolt.host/main-carrier-run!
    (fn []
      (shell/sh "true")   ; what a first call loads is loaded before any count
      (let [r (fifo "r") w (fifo "w")
            ;; the other ends: a writer and a reader that open after 300 ms (a child process, so this carrier is free)
            writer (fibers/spawn #(shell/sh "sh" "-c" (str "sleep 0.3; printf from-writer > " r)))
            read-side (ticks-during #(slurp r))
            reader (fibers/spawn #(shell/sh "sh" "-c" (str "sleep 0.3; cat " w)))
            write-side (ticks-during #(spit w "from-spit"))
            reg (str dir "/regular")
            _ (spit reg "plain")]
        {:read read-side :writer (fibers/join writer)
         :write write-side :reader (fibers/join reader)
         :regular (slurp reg)}))))

(let [[got during] (:read on-carrier)]
  (check "a read open of a FIFO answers what its writer wrote" (= got "from-writer") got)
  (check "and while it waited for a writer, another fiber on the carrier ran" (>= during 10) {:turns during}))
(let [[_ during] (:write on-carrier)]
  (check "a write open of a FIFO reaches its reader" (= (:out (:reader on-carrier)) "from-spit") (:reader on-carrier))
  (check "and while it waited for a reader, another fiber on the carrier ran" (>= during 10) {:turns during}))
(check "a regular file opens and reads as ever" (= (:regular on-carrier) "plain") (:regular on-carrier))

(shell/sh "rm" "-rf" dir)
(if (seq @failures)
  (do (doseq [f @failures] (println "FAIL:" f)) (System/exit 1))
  (println "fifo-open-parks: OK"))
