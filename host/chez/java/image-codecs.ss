;; image-codecs.ss — how the host's own values travel in a state image.
;;
;; A value whose state is a Chez structure the image cannot write as it is (a
;; hashtable keyed through jolt's hash, a chain of nodes, a tree whose comparator
;; is an internal procedure) is written through a codec (host-static.ss
;; register-image-codec!): its contents in order, as plain data, and rebuilt from
;; them by the shim's own constructor. Contents keep their order, a sorted
;; collection its comparator, a map its access order. A value that cannot travel
;; is refused by name. test/chez/image-coverage-test.ss (make imagecoverage) holds
;; every java.util collection shim to one or the other.
;;
;; Loaded after every shim it describes, before state-image.ss.

;; items as a flat list k1 v1 k2 v2 …, and back
(define (codec-kvs pairs) (fold-right (lambda (p acc) (cons (car p) (cons (cdr p) acc))) '() pairs))
(define (codec-each-kv! items f)
  (let loop ((xs items))
    (unless (null? xs) (f (car xs) (cadr xs)) (loop (cddr xs)))))

;; ---- HashMap, its look-alikes, Properties and HashSet ---------------------------
;; header #(tag defaults); a Properties keeps its defaults, not System's writethrough
(register-image-codec! "java.util.HashMap" '("hashmap" "properties")
  (lambda (m)
    (vector (vector (jhost-tag m) (or (and (string=? (jhost-tag m) "properties") (props-defaults m)) jolt-nil))
            (codec-kvs (map (lambda (k) (cons k (hashtable-ref (hm-tbl m) k jolt-nil))) (hm-keys-ordered m)))))
  (lambda (h)
    (if (string=? (vector-ref h 0) "properties")
        (make-jhost "properties" (vector (make-hashtable hm-hash jolt=2) '()
                                         (if (jolt-nil? (vector-ref h 1)) #f (vector-ref h 1)) #f))
        (make-jhost (vector-ref h 0) (vector (make-hashtable hm-hash jolt=2) '()))))
  (lambda (m items)
    (codec-each-kv! items (lambda (k v) (hm-note-key! m k) (hashtable-set! (hm-tbl m) k v)))))
(register-image-codec! "java.util.HashSet" '("hashset")
  (lambda (s) (vector "hashset" (hs->list s)))
  (lambda (h) (make-jhost h (vector (make-hashtable hm-hash jolt=2) '())))
  (lambda (s items) (for-each (lambda (x) (hm-note-key! s x) (hashtable-set! (hm-tbl s) x #t)) items)))

;; ---- LinkedHashMap / LinkedHashSet: encounter order and access order -------------
(register-image-codec! "java.util.LinkedHashMap" '("linkedhashmap")
  (lambda (m)
    (let ((c (jhost-state m)))
      (vector (lh-access? c) (codec-kvs (map (lambda (n) (cons (n-key n) (n-val n))) (lh-nodes c #f))))))
  (lambda (access?) (make-jhost "linkedhashmap" (lh-new access?)))
  (lambda (m items)
    (let ((c (jhost-state m)))
      (codec-each-kv! items (lambda (k v) (lh-insert! c #f k v #f))))))
(register-image-codec! "java.util.LinkedHashSet" '("linkedhashset")
  (lambda (s) (vector jolt-nil (map n-key (lh-nodes (jhost-state s) #f))))
  (lambda (h) (make-lhs))
  (lambda (s items) (for-each (lambda (x) (lh-insert! (jhost-state s) #f x #t #f)) items)))

;; ---- TreeMap / TreeSet: the comparator object (nil for natural order) ------------
;; The treemap and treeset tags are a whole map or set; a sub-map or a key set
;; has a tag of its own and is a view of one, refused below.
(register-image-codec! "java.util.TreeMap" '("treemap")
  (lambda (m)
    (vector (tmr-cmp-obj (v-root m))
            (codec-kvs (map (lambda (e) (cons (e-key e) (e-val e))) (v-entries m)))))
  (lambda (cmp) (tm-new-map cmp))
  (lambda (m items) (codec-each-kv! items (lambda (k v) (v-put! m k v)))))
(register-image-codec! "java.util.TreeSet" '("treeset")
  (lambda (s) (vector (tmr-cmp-obj (v-root s)) (v-keys s)))
  (lambda (cmp) (if (jolt-nil? cmp) (ts-ctor) (ts-ctor cmp)))
  (lambda (s items) (for-each (lambda (x) (ts-add! s x)) items)))

;; ---- EnumSet / EnumMap: the enum class by name, the members by ordinal ----------
(define (codec-enum-info fqn)
  (or (enum-class-info fqn)
      (jolt-throw (jolt-ex-info (string-append "image: no enum " fqn " in this build") empty-pmap))))
(register-image-codec! "java.util.EnumSet" '("enum-set-regular" "enum-set-jumbo")
  (lambda (s)
    (vector (enum-info-fqn (eset-info s))
            (let loop ((i (fx- (vector-length (eset-bits s)) 1)) (acc '()))
              (if (fx<? i 0) acc (loop (fx- i 1) (if (vector-ref (eset-bits s) i) (cons i acc) acc))))))
  (lambda (fqn) (eset-of (codec-enum-info fqn) #f))
  (lambda (s items) (for-each (lambda (i) (vector-set! (eset-bits s) i #t)) items)))
(register-image-codec! "java.util.EnumMap" '("enum-map")
  (lambda (m) (vector (enum-info-fqn (emap-info m)) (codec-kvs (emap-slots m))))
  (lambda (fqn) (emap-new (codec-enum-info fqn)))
  (lambda (m items) (codec-each-kv! items (lambda (i v) (vector-set! (emap-vals m) i v)))))

;; ---- StringBuilder / StringBuffer: the text --------------------------------------
(register-image-codec! "java.lang.StringBuilder" '("string-builder" "string-buffer")
  (lambda (sb) (vector (jhost-tag sb) (sb-str* sb)))
  (lambda (tag) (host-new (if (string=? tag "string-buffer") "java.lang.StringBuffer" "java.lang.StringBuilder")))
  (lambda (sb text) (record-method-dispatch sb "append" (jolt-list text))))

;; ---- a library's tagged table (jolt.host/tagged-table): its fields ---------------
;; A sorted collection is an htable too, and travels by its own arm (state-image.ss).
(register-image-codec-pred! "jolt.host.TaggedTable"
  (lambda (x) (and (htable? x) (not (htable-sorted? x))))
  (lambda (t)
    (vector jolt-nil
            (let-values (((ks vs) (hashtable-entries (htable-h t))))
              (codec-kvs (map cons (vector->list ks) (vector->list vs))))))
  (lambda (h) (make-htable (make-hashtable string-hash string=?)))
  (lambda (t items) (codec-each-kv! items (lambda (k v) (hashtable-set! (htable-h t) k v)))))

;; ---- what cannot travel -----------------------------------------------------------
(register-image-refusal!
  '("lhm-keyset" "lhm-values" "lhm-entryset" "lhm-reversed" "lhs-reversed" "lhm-entry"
    "enum-map-keyset" "enum-map-values" "enum-map-entryset" "enum-map-entry"
    "treemap-asc-sub" "treemap-desc-sub" "treemap-keyset" "treemap-values"
    "treemap-submap-values" "treemap-entryset" "treemap-asc-entryset" "treemap-desc-entryset"
    "treemap-entry")
  "it is a view of another collection, which the image would have to share; write the collection itself")
(register-image-refusal!
  '("arraylist-iterator" "linkedlist-iterator" "arraydeque-iterator" "arrays-aslist-iterator"
    "immutable-list-iterator" "hashset-iterator" "lhs-iterator" "lhm-keyset-iterator" "lhm-values-iterator" "lhm-entryset-iterator"
    "enum-set-regular-iterator" "enum-set-jumbo-iterator" "enum-map-keyset-iterator"
    "enum-map-values-iterator" "enum-map-entryset-iterator"
    "treemap-entry-iterator" "treemap-key-iterator" "treemap-value-iterator"
    "treemap-submap-entry-iterator" "treemap-submap-key-iterator"
    "treemap-desc-submap-entry-iterator" "treemap-desc-submap-key-iterator"
    "treemap-submap-value-iterator")
  "an iterator is a position in a walk this process is making; write the collection instead")
(register-image-refusal!
  '("user-thread" "thread-group" "executor-service" "scheduled-executor" "cf-delayed-executor"
    "future-task" "j-future" "scheduled-future" "completable-future" "process" "process-handle")
  "it is execution, not state: a thread, a pool or a task belongs to this process")
(register-image-refusal!
  '("in-stream" "out-stream" "char-reader" "char-writer" "reader-adapter" "pushback-reader"
    "line-numbering-pushback-reader" "string-reader" "print-stream" "print-writer" "port-writer"
    "random-access-file" "file-channel")
  "it is an open stream over a port this process holds; write what it reads or wrote instead")
