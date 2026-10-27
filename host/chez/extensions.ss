;; Extension points: a named contract letting core declare functionality whose
;; DATA it does not carry, and a library supply that data later.
;;
;; A point declares the type of its key, the type of its value (:fields), a TOTAL
;; :default, and what an unregistered key means (:fallback). A provider registers
;; a possibly PARTIAL value at a key; lookup is (merge default provider), so a
;; provider states only what differs, and an older provider stays valid when the
;; schema grows through refine-extension!.
;;
;;   (register-extension-point! :currency-data
;;     {:key :string :root ""
;;      :fields {:symbol :string :frac-digits :long}
;;      :default {:symbol "¤" :frac-digits 2}
;;      :fallback :strict})
;;   (register-extension! :currency-data "de" {:symbol "€"})
;;   (extension-value :currency-data "de")   ; => {:symbol "€" :frac-digits 2}
;;
;; :fallback :strict is the reason this exists rather than a map with a default.
;; It says the default answers the :root key and nothing else, so an unregistered
;; key RAISES naming the point and the key instead of quietly handing back a
;; default that is wrong for it. That is what lets core ship a point it cannot
;; populate — formatting a German amount with US separators is a silent wrong
;; answer, and refusing is the correct answer to an unanswerable question.
;;
;; Every mutation bumps extension-epoch-n. Resolution tiers above tier 3 (a plain
;; lookup) cache per call site and guard on that epoch, so a library required
;; after a site warmed up invalidates it — the same shape as jolt-proto-epoch.

;; ext-mu covers every MUTATION here: the declare's check-then-create, refine's
;; read-modify-write of the fields/default, register's provider write, and the
;; epoch bump. Declarations and registrations come from user namespaces at load,
;; and namespaces now load in parallel, so all of it runs concurrently.
;;
;; Two things break without it, and they are the two the protocol registry and the
;; multimethod epoch broke in the same way. The declare is check-then-create, so
;; two threads declaring one point each build their own providers table and a
;; provider registered through one is invisible through the other. And the epoch
;; is a `set!` read-modify-write, so a lost bump leaves a warmed call site's cached
;; resolution never invalidated — the exact staleness the epoch exists to prevent.
;;
;; Reads stay unlocked. extension-points-tbl and each providers table are STRONG
;; hashtables read single-key, which is safe for the reasons set out at var-table,
;; and jolt-extension-value is the hot path this whole epoch scheme exists to keep
;; cheap.
(define-process-local ext-mu (make-mutex))
(define extension-epoch-n 0)
;; call with ext-mu HELD
(define (bump-extension-epoch!) (set! extension-epoch-n (fx+ extension-epoch-n 1)))

(define extension-points-tbl (make-hashtable string-hash string=?))

(define-record-type ext-point
  (fields id key-kind root fallback
          hint                      ; string appended to a strict miss, or #f
          (mutable fields)          ; alist (field-name . type-name), declaration order
          (mutable default)         ; jolt map, total over fields
          providers)                ; hashtable: normalized key string -> jolt map
  (nongenerative chez-ext-point-v1))

(define (ext-bad! msg) (throw-jvm (quote IllegalArgumentException) msg))
(define (ext-get m k) (jolt-get-dispatch m k jolt-nil))
(define (ext-kw m name) (ext-get m (keyword #f name)))

;; A point id and the enum-ish spec values are keywords; render them with the
;; leading colon so an error message reads the way the source does.
(define (ext-id-str id)
  (unless (keyword? id) (ext-bad! (string-append "extension point id must be a keyword, got "
                                                 (jolt-pr-readable id))))
  (keyword-t-name id))
(define (ext-show-id id) (string-append ":" id))

(define (ext-kw-name v who)
  (unless (keyword? v) (ext-bad! (string-append who " must be a keyword, got " (jolt-pr-readable v))))
  (keyword-t-name v))

;; ---- the declared type system ----------------------------------------------
;; Deliberately small: these are the shapes host data actually takes. :any opts a
;; field out of checking rather than leaving the checker guessing.
(define (ext-type-ok? type v)
  (cond ((string=? type "string")  (string? v))
        ((string=? type "long")    (and (exact? v) (integer? v)))
        ((string=? type "double")  (flonum? v))
        ((string=? type "boolean") (boolean? v))
        ((string=? type "keyword") (keyword? v))
        ((string=? type "any")     #t)
        (else #f)))
(define (ext-known-type? type)
  (member type '("string" "long" "double" "boolean" "keyword" "any")))

(define (ext-key-kind-ok? kind v)
  (cond ((string=? kind "string")  (string? v))
        ((string=? kind "keyword") (keyword? v))
        (else #f)))
;; Providers are keyed by a normalized string so the table has one key type; the
;; point's declared :key kind is what decides whether a given value is admissible.
(define (ext-key->string point k)
  (let ((kind (ext-point-key-kind point)))
    (unless (ext-key-kind-ok? kind k)
      (ext-bad! (string-append "extension point " (ext-show-id (ext-point-id point))
                               " takes a " kind " key, got " (jolt-pr-readable k))))
    (if (string=? kind "keyword") (keyword-t-name k) k)))

;; ---- declaration ------------------------------------------------------------
;; :fields as an alist in the map's own order, each value naming a known type.
(define (ext-parse-fields fields id)
  (unless (jolt-map? fields)
    (ext-bad! (string-append "extension point " (ext-show-id id) " :fields must be a map")))
  (reverse
    (pmap-fold-fwd fields
      (lambda (k v acc)
        (let ((fname (ext-kw-name k (string-append (ext-show-id id) " field name")))
              (tname (ext-kw-name v (string-append (ext-show-id id) " field type"))))
          (unless (ext-known-type? tname)
            (ext-bad! (string-append "extension point " (ext-show-id id) " field :" fname
                                     " has unknown type :" tname)))
          (cons (cons fname tname) acc)))
      '())))

;; Check a value map against the field list. total? demands every field be present
;; (the :default contract); a provider is partial, so it only checks what it states.
(define (ext-check-value! id fields m total? what)
  (unless (jolt-map? m)
    (ext-bad! (string-append what " for extension point " (ext-show-id id) " must be a map")))
  (pmap-fold-fwd m
    (lambda (k v acc)
      (let* ((fname (ext-kw-name k (string-append what " key")))
             (fld (assoc fname fields)))
        (unless fld
          (ext-bad! (string-append what " for extension point " (ext-show-id id)
                                   " has unknown field :" fname)))
        (unless (ext-type-ok? (cdr fld) v)
          (ext-bad! (string-append what " for extension point " (ext-show-id id)
                                   " field :" fname " must be :" (cdr fld)
                                   ", got " (jolt-pr-readable v))))
        acc))
    '())
  (when total?
    (for-each (lambda (fld)
                (unless (jolt-contains? m (keyword #f (car fld)))
                  (ext-bad! (string-append what " for extension point " (ext-show-id id)
                                           " is missing field :" (car fld)
                                           " — the default must be total over :fields"))))
              fields)))

;; Re-declaring a point identically is a no-op (two loads of the same file, a
;; library declaring a point core also declares). Re-declaring it DIFFERENTLY is
;; drift — two sources disagreeing about one contract — and raises rather than
;; letting last-write-wins pick silently.
(define (ext-same-declaration? p key-kind root fallback hint fields default)
  (and (string=? (ext-point-key-kind p) key-kind)
       (string=? (ext-point-fallback p) fallback)
       (equal? (ext-point-hint p) hint)
       (jolt=2 (ext-point-root p) root)
       (equal? (ext-point-fields p) fields)
       (jolt=2 (ext-point-default p) default)))

(define (jolt-register-extension-point! id spec)
  (let* ((idn (ext-id-str id)))
    (unless (jolt-map? spec)
      (ext-bad! (string-append "extension point " (ext-show-id idn) " spec must be a map")))
    (let* ((key-kind (ext-kw-name (ext-kw spec "key") (string-append (ext-show-id idn) " :key")))
           (fallback (ext-kw-name (ext-kw spec "fallback")
                                  (string-append (ext-show-id idn) " :fallback")))
           (root (ext-kw spec "root"))
           (fields (ext-parse-fields (ext-kw spec "fields") idn))
           (default (ext-kw spec "default"))
           ;; optional: what a caller should DO about a strict miss, appended to
           ;; the raise so the message ends in an action rather than a diagnosis.
           (hint (let ((h (ext-kw spec "hint")))
                   (cond ((jolt-nil? h) #f)
                         ((string? h) h)
                         (else (ext-bad! (string-append "extension point " (ext-show-id idn)
                                                        " :hint must be a string")))))))
      (unless (member key-kind '("string" "keyword"))
        (ext-bad! (string-append "extension point " (ext-show-id idn)
                                 " :key must be :string or :keyword, got :" key-kind)))
      (unless (member fallback '("strict" "default"))
        (ext-bad! (string-append "extension point " (ext-show-id idn)
                                 " :fallback must be :strict or :default, got :" fallback)))
      (ext-check-value! idn fields default #t ":default")
      ;; :strict answers exactly one key from the default, so that key must be named
      ;; and must itself be a legal key.
      (when (string=? fallback "strict")
        (when (jolt-nil? root)
          (ext-bad! (string-append "extension point " (ext-show-id idn)
                                   " is :strict and must declare the :root key its :default answers for")))
        (unless (ext-key-kind-ok? key-kind root)
          (ext-bad! (string-append "extension point " (ext-show-id idn) " :root must be a "
                                   key-kind ", got " (jolt-pr-readable root)))))
      ;; the probe and the create are ONE step: two threads declaring the same
      ;; point must not each build a providers table, or a provider registered
      ;; through one is invisible through the other. ext-bad! throws from inside
      ;; the mutex, which with-mutex releases on the unwind.
      (jolt-with-mutex ext-mu
        (let ((prior (hashtable-ref extension-points-tbl idn #f)))
          (cond
            ((and prior (ext-same-declaration? prior key-kind root fallback hint fields default)) jolt-nil)
            (prior
             (ext-bad! (string-append "extension point " (ext-show-id idn)
                                      " is already declared with a different contract")))
            (else
             (hashtable-set! extension-points-tbl idn
               (make-ext-point idn key-kind root fallback hint fields default
                               (make-hashtable string-hash string=?)))
             (bump-extension-epoch!)
             jolt-nil)))))))

(define (ext-point-of id who)
  (let* ((idn (ext-id-str id))
         (p (hashtable-ref extension-points-tbl idn #f)))
    (unless p
      (ext-bad! (string-append who ": no extension point " (ext-show-id idn) " is declared")))
    p))

;; ---- schema refinement ------------------------------------------------------
;; Grow the value type with new OPTIONAL fields plus their defaults. Every already
;; registered provider keeps validating, and gains the new field's default through
;; the merge — refinement of the type, not a breaking redeclaration.
(define (jolt-refine-extension! id spec)
  (let* ((p (ext-point-of id "refine-extension!"))
         (idn (ext-point-id p)))
    (unless (jolt-map? spec)
      (ext-bad! (string-append "refine-extension! " (ext-show-id idn) " spec must be a map")))
    ;; The whole refinement is one critical section: it reads the point's fields
    ;; and default, merges, and writes both back, so two concurrent refines would
    ;; otherwise drop one of them and leave the point's default no longer total
    ;; over its fields.
    (jolt-with-mutex ext-mu
     (let ((new-fields (ext-parse-fields (ext-kw spec "fields") idn))
          (new-default (ext-kw spec "default")))
      ;; A field already declared may be repeated only at the same type (an
      ;; idempotent second load); at a different type it is drift.
      (for-each (lambda (fld)
                  (let ((old (assoc (car fld) (ext-point-fields p))))
                    (when (and old (not (string=? (cdr old) (cdr fld))))
                      (ext-bad! (string-append "refine-extension! " (ext-show-id idn) " field :"
                                               (car fld) " is already declared as :" (cdr old))))))
                new-fields)
      (let ((merged-fields (append (ext-point-fields p)
                                   (filter (lambda (f) (not (assoc (car f) (ext-point-fields p))))
                                           new-fields))))
        (ext-check-value! idn merged-fields new-default #f ":default")
        ;; the added fields must each carry a default, or the point's default stops
        ;; being total and a partial provider could resolve to a missing field.
        (for-each (lambda (fld)
                    (unless (or (jolt-contains? (ext-point-default p) (keyword #f (car fld)))
                                (jolt-contains? new-default (keyword #f (car fld))))
                      (ext-bad! (string-append "refine-extension! " (ext-show-id idn) " field :"
                                               (car fld) " needs a :default"))))
                  new-fields)
        (ext-point-fields-set! p merged-fields)
        (ext-point-default-set! p
          (pmap-fold-fwd new-default (lambda (k v acc) (jolt-assoc1 acc k v))
                         (ext-point-default p)))
        (bump-extension-epoch!)
        jolt-nil)))))

;; ---- registration + lookup --------------------------------------------------
(define (jolt-register-extension! id k value)
  (let* ((p (ext-point-of id "register-extension!"))
         (ks (ext-key->string p k)))
    (ext-check-value! (ext-point-id p) (ext-point-fields p) value #f "provider")
    ;; the provider write and the epoch bump together: a bump lost to a race is a
    ;; call site that keeps serving the resolution it cached before this provider
    ;; existed, with the stamp already saying current, so it never expires
    (jolt-with-mutex ext-mu
      (hashtable-set! (ext-point-providers p) ks value)
      (bump-extension-epoch!))
    jolt-nil))

(define (ext-merge default provider)
  (pmap-fold-fwd provider (lambda (k v acc) (jolt-assoc1 acc k v)) default))

(define (jolt-extension-value id k)
  (let* ((p (ext-point-of id "extension-value"))
         (ks (ext-key->string p k))
         (hit (hashtable-ref (ext-point-providers p) ks #f)))
    (cond
      (hit (ext-merge (ext-point-default p) hit))
      ((string=? (ext-point-fallback p) "default") (ext-point-default p))
      ((jolt=2 (ext-point-root p) k) (ext-point-default p))
      (else
       (ext-bad! (string-append "No " (ext-show-id (ext-point-id p)) " provider for key "
                                (jolt-pr-readable k)
                                ". The point carries a default for its root key "
                                (jolt-pr-readable (ext-point-root p))
                                " only; a library must register this key."
                                (if (ext-point-hint p)
                                    (string-append " " (ext-point-hint p))
                                    "")))))))

;; Whether a key resolves without raising — lets a caller offer a better message
;; than the raise, and lets a gate assert the tiers agree on misses too.
(define (jolt-extension-has? id k)
  (let* ((p (ext-point-of id "extension-has?"))
         (ks (ext-key->string p k)))
    (or (and (hashtable-ref (ext-point-providers p) ks #f) #t)
        (string=? (ext-point-fallback p) "default")
        (jolt=2 (ext-point-root p) k))))

(def-var! "jolt.host" "register-extension-point!" jolt-register-extension-point!)
(def-var! "jolt.host" "register-extension!" jolt-register-extension!)
(def-var! "jolt.host" "refine-extension!" jolt-refine-extension!)
(def-var! "jolt.host" "extension-value" jolt-extension-value)
(def-var! "jolt.host" "extension-has?" jolt-extension-has?)
(def-var! "jolt.host" "extension-epoch" (lambda () extension-epoch-n))
