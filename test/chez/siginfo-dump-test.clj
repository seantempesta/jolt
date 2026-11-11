;; SIGINFO's fiber dump (fibers-async.ss) reaches the process's stderr as it is now: descriptor 2,
;; never the error port the interrupted code has bound, nor the port Jolt chose at boot. A process
;; started with its two streams on one pipe gets one port for both (rt.ss jolt-install-process-streams!),
;; and a Seon agent forked from it moves its descriptor 2 to a file of its own: every dump went to the
;; other stream, the root's log, and a hung agent looked as if it had none (2026-10-28). Run with
;; descriptor 2 joined to 1 (2>&1); this program moves 2 to a file, binds the error port to a string
;; port, sends itself SIGINFO, and reads the file.
(require '[jolt.scheme :as scm])

(def file (str (System/getProperty "java.io.tmpdir") "/jolt-siginfo-dump-" (System/currentTimeMillis) ".txt"))

(def signal-into-a-bound-port!
  (scm/eval-string
    "(lambda (path)
       (let ([creat (foreign-procedure \"creat\" (string int) int)]
             [dup2 (foreign-procedure \"dup2\" (int int) int)]
             [close (foreign-procedure \"close\" (int) int)]
             [kill (foreign-procedure \"kill\" (int int) int)]
             [getpid (foreign-procedure \"getpid\" () int)]
             [captured (open-output-string)])
         ;; creat, not open: open's mode is a variadic argument, which a fixed foreign signature
         ;; passes where arm64 macOS does not read it
         (let ([fd (creat path #o644)])
           (dup2 fd 2) (close fd))
         (parameterize ([current-error-port captured])
           (kill (getpid) 29)
           ;; event checks: the queued signal's handler runs at one of them
           (let loop ([i 0] [acc 0]) (if (< i 2000000) (loop (+ i 1) (+ acc i)) acc)))
         (get-output-string captured)))"))

(defn check [what ok?] (when-not ok? (println "FAIL siginfo-dump:" what) (System/exit 1)))

(let [captured (str (signal-into-a-bound-port! file))
      written (slurp file)]
  (println "descriptor 2 got" (count written) "chars; the bound port" (count captured))
  (check "the dump reached descriptor 2" (.contains written "jolt fiber dump"))
  (check "and not the error port the interrupted code bound" (not (.contains captured "jolt fiber dump"))))

;; Each entry names where its fiber was spawned, and the running ones come first: a Seon system
;; spun one core for 16 minutes (2026-11-12) and the dump showed the two fibers that held it, one
;; running with no continuation to print and one a CPS step 'the procedure ?', and named neither.
;; A go form is named by its namespace and line; a jolt.fibers/spawn by the fn that called it; a
;; fiber running at the dump prints its own frames at its next safe point (fibers.ss
;; jolt-fiber-frames-wanted). Descriptor 2 is still the file.
(require '[clojure.core.async :as a] '[jolt.fibers :as fibers])
(defmacro line [] (:line (meta &form)))
(def spinning (atom false))
(def stop (atom false))
(defn start-spinner [] [(line) (a/go (loop [i 0] (reset! spinning true) (if @stop i (recur (inc i)))))])
(def never (a/chan))
(defn start-parked [] [(line) (a/go (a/<! never))])
(defn start-waiter [] (fibers/spawn (fn [] @(promise))))
(def signal! (scm/eval-string
               "(lambda () ((foreign-procedure \"kill\" (int int) int) ((foreign-procedure \"getpid\" () int)) 29))"))

(let [[spin-line spinner] (start-spinner)
      [park-line _] (start-parked)
      _ (start-waiter)
      from (count (slurp file))]
  (while (not @spinning) (Thread/yield))
  (Thread/sleep 20)                       ; the parked two reach their parks
  (signal!)
  ;; the gate's alarm bounds both waits: the dump's three entries, each with the line after
  ;; it, then the follow-up, which is the spinner's next preemption
  (letfn [(text [] (subs (slurp file) from))
          (entries-written? [] (re-find #"\nfiber 2: [^\n]*\n[^\n]*\n" (text)))]
    (loop [] (when-not (entries-written?) (Thread/sleep 5) (recur))))
  (let [text (subs (slurp file) from)
        lines (vec (.split text "\n"))
        entries (filterv #(re-find #"^fiber \d+: " %) lines)
        spin-site (str "spawned at user:" spin-line)]
    (println (str "the dump:\n" text))
    (check "the dump lists the carriers, each with its threads and the fiber it runs"
           (some #(re-find #"^carrier \d+: thread \d+, os thread \d+, running fiber 0$" %) lines))
    (check "the spinning go block is listed first, running, by its namespace and line"
           (let [e (first entries)] (and (.startsWith e "fiber 0: running,") (.endsWith e spin-site))))
    (check "the parked go block names its namespace and line"
           (some #(and (.contains % ": parked,") (.endsWith % (str "spawned at user:" park-line))) entries))
    (check "the spawned fiber names the fn that spawned it"
           (some #(and (.contains % ": parked,") (.endsWith % "spawned by user/start-waiter")) entries))
    (loop [] (when-not (.contains (slurp file) "at its next safe point:") (Thread/sleep 5) (recur)))
    (reset! stop true)
    (a/<!! spinner)
    (let [follow (subs (slurp file) (+ from (count text)))]
      (.delete (java.io.File. file))
      (println (str "then:\n" follow))
      (check "the running fiber printed its own frames at its next safe point, the go block's fn among them"
             (and (.startsWith follow (str "fiber 0 (running at the dump), " spin-site ", at its next safe point:\n"))
                  (.contains follow "jfn$user/start-spinner$"))))))
(println "SIGINFO-DUMP-TEST OK")
