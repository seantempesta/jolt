;; The heap ceiling's OutOfMemoryError is raised after the collection's rendezvous, never inside its
;; collect-request handler (rt.ss jolt-gc-raise-pending!). Inside it, the thread that ran the
;; collection held Chez's thread-context mutex with interrupts off, and the handlers a raise runs before
;; anything unwinds took Jolt's own locks there: one held by a thread that, converted at its own
;; collect-safe pause, was waiting for that mutex to come back from it. Neither moved again (a Seon
;; agent's test child, 2026-10-28: its collecting thread spun on a lock its main thread held, main
;; waited for the thread-context mutex). A raise-time handler reads, at the raise, whether the
;; collection is still pending and whether interrupts are off: inside the rendezvous both are so.
(require '[jolt.scheme :as scm])

(def raised-where
  (scm/eval-string
    "(lambda ()
       ;; the data is held in a box the handler empties before it escapes: the raise's continuation is
       ;; kept (the throw's record), and holding the data through it, the next form's collection met
       ;; the ceiling again (1 run in 10)
       (define held (box '()))
       (call/cc
         (lambda (k)
           (with-exception-handler
             (lambda (c)
               (set-box! held '())
               (k (format \"~a|~a|~a\"
                          (if (#%$top-level-value '$collect-request-pending) \"inside-the-collection\" \"after-it\")
                          (#3%$tc-field 'disable-count (#3%$tc))
                          (if (message-condition? c) (condition-message c) \"no message\"))))
             (lambda () (let loop () (set-box! held (cons (make-vector 4096 0) (unbox held))) (loop)))))))"))

(defn check [what ok?] (when-not ok? (println "FAIL oom-after-rendezvous:" what) (System/exit 1)))

(let [[where disabled msg] (clojure.string/split (str (raised-where)) #"\|" 3)]
  (println "raised" where "with interrupts disabled" disabled ":" msg)
  (check "the ceiling's error was raised after the collection, not inside its handler" (= "after-it" where))
  (check "with interrupts enabled" (= "0" disabled))
  (check "and it is the ceiling's" (.contains (str msg) "out of memory")))

;; and at a heap that is all other data, the stack limit in force is the stack the OS gives a thread,
;; never 0: a few frames are no overflow there (rt.ss jolt-stack-limit-for)
(def limit-at-a-full-heap
  (scm/eval-string "(lambda () (format \"~a ~a\" (jolt-stack-limit-for (expt 2 40) 65536 100663296 100663296 (jolt-stack-floor)) (jolt-stack-floor)))"))
(let [r (str (limit-at-a-full-heap))]
  (println "stack limit at a full heap, and the floor:" r)
  ;; an unlimited OS stack is no floor (jolt-rlimit-bytes answers none): nothing to check there
  (let [[a b] (re-seq #"\d+" r)]
    (when (pos? (parse-long b))
      (check "the limit at a full heap is the OS stack, not 0" (= a b)))))
;; and a thread that allocates under one of the runtime's own mutexes gets it too: held there, it was
;; never raised, and the heap was collected at its ceiling for good (the recipe's next test child)
(def under-runtime-mutex (scm/eval-string "(lambda (thunk) (let ([m (make-mutex)]) (jolt-with-mutex m (thunk))))"))
(check "an allocation under a runtime mutex gets the OutOfMemoryError"
       (= :caught (try (under-runtime-mutex (fn [] (loop [held ()] (recur (conj held (long-array 4096))))))
                       (catch OutOfMemoryError _ :caught))))
;; a Jolt catch still takes it as an OutOfMemoryError, and the heap is the program's again
(check "a catch takes the OutOfMemoryError"
       (= :caught (try (loop [held ()] (recur (conj held (long-array 4096))))
                       (catch OutOfMemoryError _ :caught))))
(check "the heap is usable after it" (= 4096 (count (long-array 4096))))
(println "OOM-AFTER-RENDEZVOUS-TEST OK")
