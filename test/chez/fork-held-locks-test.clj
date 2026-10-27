;; A child forked while another thread of its parent holds the runtime's locks still runs (locks.ss,
;; define-process-local): fork(2) copies the forking thread alone, so each lock that thread held is
;; held in the child by a thread that does not exist, and the child's first take of one waited
;; forever (a Seon agent's test child, 2026-10-28: its second thread in mutex_acquire). Here a thread
;; of the parent takes every process-wide mutex the rule registers and blocks in a read; the main thread forks
;; while it does; the child defines, loads a namespace, dispatches, matches, derefs a future and
;; dumps its fibers, each of which takes some of those locks, and exits 0. The alarm bounds a child
;; that hangs: SIGALRM ends it, red. The parent touches no lock of its own from the hold to the
;; child's end: the whole of it is one Scheme procedure.
(require '[jolt.scheme :as scm])

(def fork-beside-held-locks
  (scm/eval-string
    "(let ([fork (foreign-procedure \"fork\" () int)]
           [pipe (foreign-procedure \"pipe\" (u8*) int)]
           [rd (foreign-procedure __collect_safe \"read\" (int u8* size_t) ssize_t)]
           [wr (foreign-procedure \"write\" (int u8* size_t) ssize_t)]
           [waitpid (foreign-procedure __collect_safe \"waitpid\" (int u8* int) int)]
           [yield (foreign-procedure \"sched_yield\" () int)]
           [alarm (foreign-procedure \"alarm\" (unsigned) unsigned)]
           ;; every lock the rule covers, as registered (locks.ss jolt-process-locals): a mutex, or a
           ;; vector of them; each once
           [mutexes (let ([seen (make-eq-hashtable)])
                      (filter (lambda (m) (and (mutex? m) (not (hashtable-contains? seen m)) (begin (hashtable-set! seen m #t) #t)))
                              (apply append (map (lambda (v) (if (vector? v) (vector->list v) (list v))) (jolt-process-locals)))))])
       (define (pipe-pair) (let ([b (make-bytevector 8)]) (pipe b) (cons (bytevector-s32-native-ref b 0) (bytevector-s32-native-ref b 4))))
       (case-lambda
        [(count?) (length mutexes)]
        [()
         ;; the parent too: a holder that cannot take a lock, or a child that never ends, is red, not a hang
         (alarm 120)
         (let ([held (pipe-pair)] [release (pipe-pair)] [one (make-bytevector 1)])
           ;; the holder: a Chez thread (no jolt lock taken to start it), every lock, then a read
           (#%fork-thread
             (lambda ()
               (for-each mutex-acquire mutexes)
               (wr (cdr held) one 1)
               (rd (car release) one 1)
               (for-each mutex-release mutexes)))
           (rd (car held) one 1)
           ;; the fork, once the holder is parked in its read and so not an active thread
           (let ([pid (let retry ()
                        (disable-interrupts)
                        (mutex-acquire #%$tc-mutex)
                        (let ([pid (if (= (#%$top-level-value '$active-threads) 1) (fork) -2)])
                          (mutex-release #%$tc-mutex)
                          (enable-interrupts)
                          (if (= pid -2) (begin (yield) (retry)) pid)))])
             (if (= pid 0)
                 (begin (alarm 30) (jolt-after-fork-child!) -1)
                 (let ([st (make-bytevector 4)])
                   (waitpid pid st 0)
                   (wr (cdr release) one 1)
                   (alarm 0)
                   (bytevector-s32-native-ref st 0)))))]))"))

;; what it holds is the registry, and the registry is the runtime's ~100 locks, not an empty list
(def held-count (fork-beside-held-locks true))

(let [r (fork-beside-held-locks)]
  (if (= r -1)
    ;; the child
    (do (eval '(def forked-child-var 1))
        (load-string "(ns fork-held-locks-child) (defmulti m identity) (defmethod m 1 [_] :one)
                      (defprotocol P (p [x])) (defrecord R [a] P (p [_] a))
                      (defn run [] [(m 1) (p (->R 2)) (str (reify Object (toString [_] \"r\")))])")
        (let [v ((resolve 'fork-held-locks-child/run))
              ok (and (= v [:one 2 "r"])
                      (some? (gensym))
                      (= "aa" (re-find #"a+" "caa"))
                      (= :k1 (keyword (str "k" 1)))
                      (= 3 @(future (+ 1 2)))
                      (string? (jolt.host/fiber-dump))
                      (= "{:a 1}" (pr-str {:a 1})))]
          (System/exit (if ok 0 3))))
    (do (if (and (zero? r) (>= held-count 100))
          (println "FORK-HELD-LOCKS-TEST OK:" held-count "locks held across the fork")
          (println "FORK-HELD-LOCKS-TEST FAILED: held" held-count "locks; the child's wait status" r "(14 = SIGALRM: it hung on a lock it inherited held)"))
        (flush)
        (System/exit (if (and (zero? r) (>= held-count 100)) 0 1)))))
