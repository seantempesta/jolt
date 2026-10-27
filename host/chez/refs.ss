;; refs.ss — Clojure refs and STM for the Chez host.
;;
;; A single global transaction lock gives correct serializable semantics on
;; jolt's shared-heap threads — no MVCC needed.  Transactions buffer writes
;; in a per-txn log and only commit (write ref values) on success, providing
;; rollback on exception.  Watches fire once per changed ref after commit,
;; outside the transaction lock, matching JVM semantics.
;;
;; The lock IS the isolation, and it is an object MONITOR rather than an OS mutex
;; because a mutex cannot carry that across a fiber park — see stm-lock below
;; (jolt-pb2s), and dyn-with-txn for the other half, which is *txn* itself
;; (jolt-49ay).
;;
;; Refs participate in the IRef seam (watches/validators/metadata) like
;; atom/var/agent.  Loaded after atoms.ss (shares jolt-iref-state-throw and
;; the iref tables).

;; Refs travel in state images by VALUE (state-image.ss substitutes an
;; image-ref descriptor on dump and re-mints through make-jolt-ref on
;; restore), so this layout is NOT image-format surface and may change
;; freely. The uid jolt-ref-v1 is RETIRED: format-2 images carry refs as raw
;; nongenerative jolt-ref-v1 records (fields: val lock), and the legacy
;; restore arm depends on materializing that rtd from the fasl without a
;; live conflict — never reuse the v1 uid for a different layout.
(define-record-type jolt-ref
  (fields (mutable val))
  (nongenerative jolt-ref-v2))

;; IRef arm: refs are watchable/validatable through the iref tables.
(register-iref-arm! jolt-ref?)

;; Per-ref min/max history (defaults 0 and 10), stored in weak side tables.
;; ref-min-history / ref-max-history live in side-tables rather than ref fields.
;; A Chez hashtable is not thread-safe and STM is concurrent by definition, so
;; both go through one mutex: unsynchronized mutation corrupts the table and the
;; damage surfaces as a fault inside the collector, naming nothing. Cold on both
;; sides — these are only touched by the user-facing getter/setter, never by the
;; commit path (jolt-ref-history-count is 0; history is not implemented) — so the
;; lock costs nothing measurable.
(define ref-min-history-tbl (make-weak-eq-hashtable))
(define ref-max-history-tbl (make-weak-eq-hashtable))
(define-process-local ref-history-mu (make-mutex))

;; --- transaction record -------------------------------------------------------
;; A per-transaction record, held in the thread-parameter *txn* (#f when
;; no transaction is running).
;;   log         — eq-hashtable: ref -> in-txn value
;;   old-vals    — eq-hashtable: ref -> committed value at first mutation
;;                 (captured once per ref for the single watch notification)
;;   pending-sends — list of (agent f args) enqueued during this txn (Round 3)
(define-record-type jolt-txn
  (fields (mutable log) (mutable old-vals) (mutable pending-sends))
  (nongenerative jolt-txn-v1))

(define (make-txn)
  (make-jolt-txn (make-eq-hashtable) (make-eq-hashtable) '()))

;; --- transaction state -------------------------------------------------------
;; A single global lock serializes all transactions.  Per-thread *txn* detects
;; nested dosync (joins the outer transaction) and guards against io!/ref-set/
;; alter/commute/ensure outside a transaction.
;;
;; AN OBJECT MONITOR AND NOT A MUTEX, and that is the isolation (jolt-pb2s).
;;
;; This was (define stm-lock (make-mutex)) taken through jolt-with-mutex around the
;; whole transaction body. An OS mutex has THREAD granularity and a fiber is not a
;; thread, so it cannot carry an invariant across a park — and jolt-with-mutex is a
;; dynamic-wind, so a fiber that parks inside the body RELEASES it on the way out
;; and re-acquires it on resume. That is exactly what locks.ss says makes parking
;; inside jolt-with-mutex safe, and exactly what a transaction cannot survive:
;; another transaction ran and COMMITTED inside this one's extent, and this one then
;; committed writes derived from reads that predated it. Measured: a fiber reading
;; r = 0, parking, and writing read+1 while a second transaction committed r = 100
;; left r = 1, where the only serializable outcomes are 100 and 101.
;;
;; There is nothing downstream to catch it. jolt's STM has no MVCC and no retry —
;; the lock IS the isolation — so dropping it mid-body drops the only mechanism
;; there is, silently. And this is a scenario the design commits to rather than one
;; to rule out: fibers.ss carries *txn* in the per-fiber dynamic slice precisely so
;; that a fiber parked inside a dosync resumes inside its txn.
;;
;; So exclusion is carried the way jolt-3a87 carries an object monitor's: by an
;; OWNER FIELD keyed on the execution context, which a context switch cannot
;; disturb because it is not a wind. jolt-with-monitor is that lock already — its
;; bookkeeping mutex is held only across the enter/exit decision, a fiber contender
;; PARKS on the monitor's waiter list (never a condition-wait, which would block the
;; carrier the holder may need in order to reach its commit) and a thread waits on
;; its condition, and its after-thunk asks jolt-park-unwinding? so a park keeps
;; ownership. Nothing here has to know any of that; it just has to stop using a
;; mutex for a region that spans user code.
;;
;; The second thing this buys is jolt-d7l5: a monitor holds no COUNTED lock while
;; the body runs, so a fiber inside a transaction is preemptible again. Under the
;; mutex it was not, for the whole body, which is reason 3 of the three ways the
;; monitor itself failed — an unbounded starvation window over arbitrary user code.
;;
;; WHAT IT TRADES. Two transactions that today interleave over a park will now
;; serialize, and a program relying on that interleaving to make progress can
;; deadlock instead. That is jolt-3a87's call again: a hang is diagnosable and a
;; lost update is not.
;;
;; The lock-order rule the loader rests on is unchanged — a load must never acquire
;; this lock (loader.ss ldr-libs-update!), which is what keeps stm-lock -> ns_load
;; acyclic — and it is if anything easier to hold now, since a fiber contender parks
;; instead of pinning its carrier.
;;
;; jolt-with-monitor lives in java/concurrency.ss, which rt.ss loads AFTER this
;; file; the reference resolves at call time, the same forward reference
;; jolt-alter-var-root makes from dyn-binding.ss and jolt-run-interruptible makes to
;; fibers.ss. Nothing runs a transaction before the boot finishes loading.
(define stm-lock (vector 'stm-lock))
(define *txn* (make-thread-parameter #f))

;; (dyn-with-txn txn thunk) — scope *txn* over a dynamic extent.
;;
;; NOT `parameterize`, and the reason is the rule dyn-binding.ss already states at
;; dyn-with-frame: a winder must not re-establish state the jolt-dslice already
;; carries (jolt-49ay). The dslice carries *txn* — that is how a fiber parked inside
;; a dosync resumes inside its own transaction — and Chez's parameterize is a SWAP:
;; one thunk is both the wind's before and its after, exchanging the parameter with a
;; saved slot. jolt-fiber-slice-restore! writes *txn* BEFORE the rewind runs that
;; swap, so the saved slot came back holding the transaction instead of the outer
;; nil, and the way out then restored the TRANSACTION. The fiber left its dosync
;; still inside one, and its next dosync saw (*txn*) non-nil, took the nested arm,
;; joined the dead transaction, took no lock at all and wrote into a log nobody would
;; ever commit. Measured with no preemption at all: three parked transactions in a
;; row committed once; under contention, 396 of 400 dosync calls joined a stale txn
;; and four transaction objects served four hundred transactions.
;;
;; So the same shape dyn-with-frame uses, for the same reason. The set happens ONCE,
;; outside the wind, so a rewind cannot re-establish anything; and the after-thunk
;; restores an ABSOLUTE value rather than swapping, so it is right however many times
;; the extent is left and re-entered. The slice is then the single owner of *txn*
;; across a switch, which is what fibers.ss says the design is.
;;
;; This is the ONLY parameterize the runtime had over a slice-carried parameter. The
;; others are over parameters the slice does not touch (the reader's mode, the
;; loader's sinks and flags), where the swap is sound because nothing else writes
;; them while the extent is unwound.
;;
;; Note the after-thunk here DOES fire on a park, which is the opposite of
;; jolt-with-monitor's just above it in jolt-sync — and the difference is which
;; mechanism owns the state across the switch. The slice owns *txn*, so letting it
;; revert and be restored is correct; the monitor owns its ownership field, so a park
;; must not release it. Both rest on a park being a CAPTURE park that rewinds, which
;; is what fn* opacity gives dosync (java/sm.ss's invariant; run-gosm.ss checks it).
(define (dyn-with-txn txn thunk)
  (let ((outer (*txn*)))
    ;; masked at its edges (jolt-wind*, locks.ss): an interrupt's escape at either
    ;; left *txn* set, and the thread ran on as if inside the transaction
    (jolt-wind* (lambda () #f) (*txn* txn) thunk (*txn* outer))))

;; --- in-txn log helpers ------------------------------------------------------

;; Sentinel for hashtable-ref to distinguish "not found" from a valid value.
(define txn-not-found (list 'txn-not-found))

;; Read a ref's in-txn value from the transaction log, falling back to the
;; ref's committed value if this txn has not touched it.
(define (txn-read ref)
  (let ((txn (*txn*)))
    (if txn
        (let ((v (hashtable-ref (jolt-txn-log txn) ref txn-not-found)))
          (if (eq? v txn-not-found) (jolt-ref-val ref) v))
        (jolt-ref-val ref))))

;; Write a ref's in-txn value to the log.  Captures the pre-txn committed
;; value on first mutation if not already recorded.
(define (txn-write! ref v)
  (let* ((log (jolt-txn-log (*txn*)))
         (ov (jolt-txn-old-vals (*txn*))))
    ;; capture pre-txn value on first write to this ref
    (unless (hashtable-contains? ov ref)
      (hashtable-set! ov ref (jolt-ref-val ref)))
    (hashtable-set! log ref v)))

;; Commit: write all buffered values to the refs.  Must be called while
;; still holding stm-lock.
(define (txn-commit! txn)
  (let ((log (jolt-txn-log txn)))
    (vector-for-each
      (lambda (ref) (jolt-ref-val-set! ref (hashtable-ref log ref #f)))
      (hashtable-keys log))))

;; Fire watch notifications for committed changes.  Called AFTER releasing
;; stm-lock and clearing *txn*, so watches can open their own dosync.
(define (txn-fire-watches! txn)
  (let ((log (jolt-txn-log txn))
        (ov (jolt-txn-old-vals txn)))
    (vector-for-each
      (lambda (ref)
        (let ((new-val (hashtable-ref log ref #f))
              (old (hashtable-ref ov ref txn-not-found)))
          (unless (eq? old txn-not-found)
            (iref-notify ref old new-val))))
      (hashtable-keys log))))

;; --- constructor -------------------------------------------------------------
;; (ref init :validator f :meta m) — the ARef ctor contract: validator runs
;; against the initial value; :meta must be a map.
(define (jolt-ref-new v . opts)
  (let loop ((o opts) (validator jolt-nil) (m #f))
    (cond
      ((or (null? o) (null? (cdr o)))
       (let ((r (make-jolt-ref v)))
         ;; validate init via iref validator table
         (when (and (not (jolt-nil? validator)) (jolt-not (jolt-invoke validator v)))
           (jolt-iref-state-throw))
         (unless (jolt-nil? validator)
           (hashtable-set! iref-validator-tbl r validator))
         (when (and m (not (jolt-nil? m)))
           (unless (jolt-map? m)
             (jolt-throw (jolt-host-throwable
                          "java.lang.ClassCastException"
                          (string-append "class " (jolt-class-name m)
                                         " cannot be cast to class clojure.lang.IPersistentMap"))))
           (meta-table-set! r m))
         r))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "validator"))
       (loop (cddr o) (cadr o) m))
      ((and (keyword-t? (car o)) (string=? (keyword-t-name (car o)) "meta"))
       (loop (cddr o) validator (cadr o)))
      (else (loop (cddr o) validator m)))))

;; --- transaction-guarded operations ------------------------------------------
;; Inside a transaction, ref-set/alter/commute/ensure write through the
;; per-txn log; on commit the buffered values are written to the refs.
;; On exception the log is discarded — rollback is implicit.

(define (jolt-ref-ensure-txn)
  (unless (*txn*)
    (jolt-throw (jolt-host-throwable
                 "java.lang.IllegalStateException"
                 "No transaction running"))))

(define (jolt-ref-set ref v)
  (jolt-ref-ensure-txn)
  (iref-validate ref v)
  (txn-write! ref v)
  v)

(define (jolt-alter ref f . args)
  (jolt-ref-ensure-txn)
  (let* ((old (txn-read ref))
         (v (apply jolt-invoke f old args)))
    (iref-validate ref v)
    (txn-write! ref v)
    v))

;; Under serialized transactions, commute is equivalent to alter (no
;; commutative optimization needed).
(define (jolt-commute ref f . args)
  (apply jolt-alter ref f args))

;; ensure: under serialized transactions this is a no-op beyond the
;; transaction-enforcement guard — there is no ref to "touch" because no
;; other thread can mutate it while we hold the global lock.
(define (jolt-ensure ref)
  (jolt-ref-ensure-txn)
  (txn-read ref))

;; __sync-call: run a thunk inside a serialized transaction — the seam the
;; sync/dosync MACROS (30-macros.clj) expand through; sync itself is a macro
;; with the reference's (sync flags & body) shape.  Nested calls join the
;; outer transaction (re-entrant through the thread-local parameter).
(define (jolt-sync thunk)
  (if (*txn*)
      ;; nested — just run the body under the existing transaction
      (jolt-invoke thunk)
      ;; outer transaction: acquire lock, buffer writes, commit or rollback
      (let ((txn (make-txn))
            (aborted #f)
            (result #f))
        ;; The body is handed over as a thunk because that is what a monitor takes,
        ;; and dosync already reaches here as (__sync-call (fn* [] body)) — an fn*,
        ;; which is opaque to the CPS pass, so a park inside a transaction takes a
        ;; CAPTURE park and rewinds properly. jolt-with-monitor's after-thunk needs
        ;; that: a CHEAP park never rewinds, so it would skip the release and leave
        ;; the transaction lock held for the life of the process.
        (jolt-with-monitor stm-lock
          (lambda ()
            (dyn-with-txn txn
              (lambda ()
                (guard (e (#t (set! aborted #t) (set! result e)))
                  (set! result (jolt-invoke thunk)))
                (unless aborted
                  (txn-commit! txn))))))
        ;; the monitor is released and *txn* is back to #f
        (unless aborted
          (txn-fire-watches! txn)
          ;; dispatch deferred agent sends inside a txn.  Look up send from
          ;; clojure.core (resolved at runtime, after concurrency.ss loads).
          (let ((sends (jolt-txn-pending-sends txn)))
            (when (pair? sends)
              (let ((send-fn (or jolt-txn-send-fn
                                 (let ((v (var-deref "clojure.core" "send")))
                                   (set! jolt-txn-send-fn v) v))))
                (for-each (lambda (entry)
                            (apply send-fn entry))
                          (reverse sends))))))
        (if aborted (raise result) result))))

;; io! is a MACRO (30-macros.clj): its body must NOT evaluate when the
;; transaction check throws. The macro tests this seam.
(define (jolt-txn-running?) (and (*txn*) #t))

;; --- history ops -------------------------------------------------------------
;; On the JVM these control how many prior values a ref keeps for snapshot
;; isolation: a reader in a transaction that started before a writer committed
;; is served an older value out of the history chain instead of retrying.
;;
;; jolt's transactions are SERIALIZED by stm-lock, so there is no such reader —
;; a transaction never observes a commit inside its own extent, which is what
;; the history chain exists to paper over. There is therefore nothing to store
;; and nothing for the commit path to store it in, and ref-history-count is 0
;; where the JVM reports the chain's length (recorded as an accepted
;; :concurrency-model divergence, not a gap to close).
;;
;; The min/max settings themselves stay a faithful GETTER/SETTER pair — the
;; getter returns what the setter stored (defaults 0 and 10, as on the JVM) and
;; the setter returns the ref — so code that tunes a ref keeps working and reads
;; back what it set. They just do not feed a mechanism jolt has.
;;
;; case-lambda rather than a rest-arg: a rest-arg accepted ANY arg count and
;; silently took the getter branch for 3+ (so (ref-min-history r 1 2) read the
;; value instead of raising), and (ref-min-history) died in `car` with a raw
;; host error that carried no class, so no catch clause could select it.
;; nil and a wrong type are different answers on the JVM, because the ^Ref hint
;; makes these an interface call: a null target is a NullPointerException, a
;; non-Ref is the cast failure. jolt-class-name itself raises on nil, so the nil
;; arm has to come first regardless.
(define (ref-history-arg-check ref)
  (cond
    ((jolt-ref? ref) #t)
    ((jolt-nil? ref)
     (throw-jvm (quote NullPointerException)
                "Cannot invoke \"clojure.lang.Ref\" method because \"ref\" is null"))
    (else
     (jolt-throw (jolt-host-throwable
                  "java.lang.ClassCastException"
                  (string-append "class " (jolt-class-name ref)
                                 " cannot be cast to class clojure.lang.Ref"))))))
(define (jolt-ref-history-count ref)
  (ref-history-arg-check ref)
  0)

(define (make-ref-history-op tbl default)
  (case-lambda
    ((ref)
     (ref-history-arg-check ref)
     (jolt-with-mutex ref-history-mu (hashtable-ref tbl ref default)))
    ((ref n)
     (ref-history-arg-check ref)
     (jolt-with-mutex ref-history-mu (hashtable-set! tbl ref n))
     ref)))
(define jolt-ref-min-history (make-ref-history-op ref-min-history-tbl 0))
(define jolt-ref-max-history (make-ref-history-op ref-max-history-tbl 10))

;; --- deref -------------------------------------------------------------------
;; Inside a transaction, return the in-txn value from the log (falling back
;; to the ref's committed value).  Outside a transaction, return the ref's
;; committed value directly.
(define (jolt-ref-deref ref)
  (txn-read ref))

;; Chain jolt-deref to handle refs (capture the pre-ref jolt-deref from atoms).
(define %pre-ref-deref jolt-deref)
(set! jolt-deref
  (lambda (x . opts)
    (if (jolt-ref? x)
        (jolt-ref-deref x)
        (apply %pre-ref-deref x opts))))

;; --- bind into clojure.core -------------------------------------------------
;; sync/dosync/io! are macros in the overlay (30-macros.clj) over the
;; __sync-call / __txn-running? seams.
(def-var! "clojure.core" "ref" jolt-ref-new)
(def-var! "clojure.core" "ref?" jolt-ref?)
(def-var! "clojure.core" "ref-set" jolt-ref-set)
(def-var! "clojure.core" "alter" jolt-alter)
(def-var! "clojure.core" "commute" jolt-commute)
(def-var! "clojure.core" "ensure" jolt-ensure)
(def-var! "clojure.core" "__sync-call" jolt-sync)
(def-var! "clojure.core" "__txn-running?" jolt-txn-running?)
(def-var! "clojure.core" "ref-history-count" jolt-ref-history-count)
;; ref-min-history / ref-max-history are multi-arity (getter / setter)
(def-var! "clojure.core" "ref-min-history" jolt-ref-min-history)
(def-var! "clojure.core" "ref-max-history" jolt-ref-max-history)
;; deref is already bound; the chain above extends jolt-deref, and
;; concurrency.ss will re-chain over us.

;; *loaded-libs* — seeded empty; loader.ss populates and wires it.
(def-dynvar! "clojure.core" "*loaded-libs*" (jolt-ref-new (jolt-hash-set)))
;; loaded-libs fn returns the derefed set.
(def-var! "clojure.core" "loaded-libs"
  (lambda () (jolt-deref (var-deref "clojure.core" "*loaded-libs*"))))

;; Cached send fn for dispatching deferred agent sends after txn commit.
;; Resolved lazily at runtime (after concurrency.ss has loaded send into core).
(define jolt-txn-send-fn #f)
