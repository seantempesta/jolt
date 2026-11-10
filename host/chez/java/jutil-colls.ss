;; jutil-colls.ss — what makes a mutable java.util collection shim a
;; java.util.Map / Set / List to the rest of the runtime: = against the
;; persistent collections, hash, pr and str, reduce-kv, seqable?. Shared by
;; every target. Each shim registers its tag here with its KIND and a procedure
;; listing its live elements in iteration order; nothing below knows how a shim
;; stores them. host-static-classes.ss registers HashMap / HashSet / the
;; ArrayList family, tree-map.ss the TreeMap and TreeSet views.
;;
;; The JVM answers these questions from the interfaces, not the classes, which
;; is why one registry serves them all:
;;   - APersistentMap/Set/Vector.equiv accept any java.util.Map/Set/List, and a
;;     java.util collection's own equals accepts a persistent one, so (= hm {..})
;;     holds from both sides. Only a List is sequential: an ArrayDeque is a
;;     Collection and compares by identity.
;;   - hash of a non-IHashEq object is its hashCode, the java.util contract
;;     (Map: sum of key^value, Set: sum, List: 31*h+e), NOT the hasheq of the
;;     equal persistent collection.
;;   - pr prints a Map as a map literal, a Set as #{}, a RandomAccess List as
;;     [], any other List as (); print (readably false) and anything else is
;;     #object[class toString]; str is toString — {k=v, …} or [a, b].
;;
;; Needs the jhost record and register-host-methods! (host-static.ss on Chez,
;; host/gambit/host-statics.ss), the arm registries (values.ss, printing.ss,
;; converters.ss) and jolt-java-hashcode (natives-misc.ss). Loads before
;; host-static-classes.ss, which registers into it.

;; ---- the registry -------------------------------------------------------------
;; tag -> #(kind elems). kind is one of
;;   map     java.util.Map; elems answers map entries
;;   set     java.util.Set
;;   entries a Map's entrySet: a Set whose members are map entries, which
;;           hash and print the way Map.Entry does (k^v, k=v)
;;   ralist  a RandomAccess java.util.List (ArrayList, Arrays$ArrayList)
;;   list    any other java.util.List (LinkedList)
;;   coll    a Collection that is none of those (ArrayDeque): str only
;; The Map$Entry host objects below share the table under the kind entry, so
;; =, hash and str each walk ONE arm for every java.util host value — the arm
;; walk is paid by every value no fast path claims (a uuid's hash, an atom's =).
(define jutil-colls-tbl (make-hashtable string-hash string=?))
(define (register-jutil-coll! tag kind elems)
  (hashtable-set! jutil-colls-tbl tag (vector kind elems))
  (register-host-methods! tag (jutil-object-methods kind)))
;; The last tag looked up, as one (tag-object . entry) pair: a pair is published
;; by a single reference write, so a racing reader sees the old pair or the new
;; one, never half of either. A pr or = over a run of the same shim then skips
;; the string hash, which is most of what the arm costs.
(define jutil-tag-cache (cons #f #f))
(define (jutil-tag-entry x)
  (and (jhost? x)
       (let ((tag (jhost-tag x)) (c jutil-tag-cache))
         (if (eq? (car c) tag)
             (cdr c)
             (let ((e (hashtable-ref jutil-colls-tbl tag #f)))
               (set! jutil-tag-cache (cons tag e))
               e)))))
(define (jutil-any-kind x)
  (let ((e (jutil-tag-entry x))) (and e (vector-ref e 0))))
;; the collection kind, or #f — never entry
(define (jutil-coll-kind x)
  (let ((k (jutil-any-kind x))) (and k (not (eq? k 'entry)) k)))
(define (jutil-coll-elems x) ((vector-ref (jutil-tag-entry x) 1) x))
;; a shim the JVM compares by contents (everything but a bare Collection)
(define (jutil-equiv-coll? x)
  (let ((k (jutil-coll-kind x))) (and k (not (eq? k 'coll)))))
;; what the = and hash arms claim: a content-compared shim or an entry
(define (jutil-equiv-value? x)
  (let ((k (jutil-any-kind x))) (and k (not (eq? k 'coll)))))
;; Iterable on the JVM: seq, seqable?, reduce walk it. post-prelude's seqable?
;; consults this.
(define (jhost-seqable-shim? x) (and (jutil-coll-kind x) #t))

;; ---- equality -----------------------------------------------------------------
;; A shim compares as the persistent collection with the same contents, which is
;; exactly the java.util contract the persistent side checks on the JVM (a Map's
;; entries, a Set's members, a List's elements in order). The converted value is
;; never a shim, so the re-dispatch cannot come back here.
(define (jutil-coll->persistent x)
  (case (jutil-coll-kind x)
    ((map) (fold-left (lambda (m e) (pmap-assoc m (jolt-nth e 0) (jolt-nth e 1)))
                      empty-pmap (jutil-coll-elems x)))
    ((set entries) (fold-left pset-conj empty-pset (jutil-coll-elems x)))
    ((ralist list) (apply jolt-vector (jutil-coll-elems x)))
    (else x)))
(define (jutil-plain x) (if (jutil-equiv-coll? x) (jutil-coll->persistent x) x))
(define (jutil-equals? a b)
  (cond ((or (eq? (jutil-any-kind a) 'entry) (eq? (jutil-any-kind b) 'entry))
         (jutil-entry-equals? a b))
        ((jolt=2 (jutil-plain a) (jutil-plain b)) #t)
        (else #f)))
(register-eq-arm! (lambda (a b) (or (and (jhost? a) (jutil-equiv-value? a))
                                    (and (jhost? b) (jutil-equiv-value? b))))
                  jutil-equals?)

;; ---- hashCode -----------------------------------------------------------------
(define (jutil-hash-code x)
  (case (jutil-any-kind x)
    ((entry) (jutil-entry-hash x))
    ((map entries) (fold-left (lambda (h e)
                        (i32 (+ h (bitwise-xor (jolt-java-hashcode (jolt-nth e 0))
                                               (jolt-java-hashcode (jolt-nth e 1))))))
                      0 (jutil-coll-elems x)))
    ((set) (fold-left (lambda (h e) (i32 (+ h (jolt-java-hashcode e)))) 0 (jutil-coll-elems x)))
    (else (fold-left (lambda (h e) (i32 (+ (* 31 h) (jolt-java-hashcode e)))) 1
                     (jutil-coll-elems x)))))
;; jhost? is asked inline: this arm sits in the walk every unclaimed value's
;; hash pays (a uuid's, a record's), so a non-host value must leave in one test.
(register-hash-arm! (lambda (x) (and (jhost? x) (jutil-equiv-value? x))) jutil-hash-code)

;; ---- toString -----------------------------------------------------------------
;; String.valueOf of each element: null is "null", a collection holding itself
;; renders "(this Map)" / "(this Collection)" instead of recursing forever.
(define (jutil-elem-string self x)
  (cond ((eq? x self) (if (eq? (jutil-coll-kind self) 'map) "(this Map)" "(this Collection)"))
        ((string? x) x)
        ((jolt-nil? x) "null")
        (else (jolt-str-one x))))
(define (jutil-ek e) (if (pvec? e) (pvec-nth-d e 0 jolt-nil) (jolt-nth e 0)))
(define (jutil-ev e) (if (pvec? e) (pvec-nth-d e 1 jolt-nil) (jolt-nth e 1)))
(define (jutil-entry-string self e)
  (string-append (jutil-elem-string self (jutil-ek e)) "=" (jutil-elem-string self (jutil-ev e))))
(define (jutil-to-string x)
  (let* ((te (jutil-tag-entry x)) (kind (vector-ref te 0)))
    (case kind
      ((entry) (jutil-entry-string-of x))
      ((map) (string-append "{"
               (jolt-str-join-comma (map (lambda (e) (jutil-entry-string x e)) ((vector-ref te 1) x)))
               "}"))
      (else (string-append "["
              (jolt-str-join-comma
                (map (if (eq? kind 'entries)
                         (lambda (e) (jutil-entry-string x e))
                         (lambda (e) (jutil-elem-string x e)))
                     ((vector-ref te 1) x)))
              "]")))))
(register-str-render! jutil-any-kind jutil-to-string)

;; ---- pr -----------------------------------------------------------------------
;; The readable printer's map/set/list shapes, honoring *print-length* and
;; *print-level* like the persistent ones. A java.util.Map does not lift a shared
;; namespace (print-map, not the IPersistentMap method). Printed with
;; *print-readably* off — print / println — the JVM falls back to #object, which
;; is jolt-object-repr over the toString above.
(define (jutil-pr x)
  (if (not (jolt-pr-readable?))
      (jolt-object-repr x #f)
      (let* ((te (jutil-tag-entry x)) (kind (vector-ref te 0)))
        (if (jolt-print-hash?) "#"
            (with-deeper-print
              (let ((xs ((vector-ref te 1) x)))
                (case kind
                  ((map) (string-append "{"
                           (jolt-str-join-comma
                             (jutil-limited-strs xs
                               (lambda (e) (string-append (jolt-pr-readable (jutil-ek e)) " "
                                                          (jolt-pr-readable (jutil-ev e))))))
                           "}"))
                  ((set entries) (string-append "#{" (jolt-str-join (jutil-limited-strs xs jolt-pr-readable)) "}"))
                  ((ralist) (string-append "[" (jolt-str-join (jutil-limited-strs xs jolt-pr-readable)) "]"))
                  (else (string-append "(" (jolt-str-join (jutil-limited-strs xs jolt-pr-readable)) ")")))))))))
;; render at most *print-length* of a list, then "..."
(define (jutil-limited-strs xs render)
  (let ((lim (jolt-print-length)))
    (let loop ((xs xs) (i 0) (acc '()))
      (cond ((null? xs) (reverse acc))
            ((and lim (fx>=? i lim)) (reverse (cons "..." acc)))
            (else (loop (cdr xs) (fx+ i 1) (cons (render (car xs)) acc)))))))
(register-pr-readable-arm! jutil-equiv-coll? jutil-pr)

;; ---- reduce-kv ----------------------------------------------------------------
;; IKVReduce is extended to java.util.Map: (reduce-kv f init a-HashMap) walks its
;; entries (seq.ss consults this hook before refusing the value).
(set! jolt-reduce-kv-entries
  (lambda (x) (and (eq? (jutil-coll-kind x) 'map) (jutil-coll-elems x))))

;; ---- the Object methods every registered shim answers --------------------------
(define (jutil-object-methods kind)
  (cons (cons "toString" (lambda (self) (jutil-to-string self)))
        (if (eq? kind 'coll)
            '()
            (list (cons "equals" (lambda (self o) (jutil-equals? self o)))
                  (cons "hashCode" (lambda (self) (jutil-hash-code self)))))))

;; ---- a proxy over a shim ---------------------------------------------------------
;; A proxy over a java.util collection is that collection to count, seq, get and
;; contains?, which reach it through the methods the JVM's RT does — size,
;; containsKey / contains, get — so an override in the proxy body answers.
(define (proxy-coll-kind x)
  (let ((d (and (jreify? x) (jreify-delegate x))))
    (and d (jutil-coll-kind d))))
(define (proxy-call x m . args) (record-method-dispatch x m (if (null? args) jolt-nil (list->cseq args))))
(register-count-arm! proxy-coll-kind (lambda (x) (jnum->exact (proxy-call x "size"))))
(register-seq-arm! proxy-coll-kind (lambda (x) (jolt-seq (jreify-delegate x))))
(register-get-arm! (lambda (x) (eq? (proxy-coll-kind x) 'map))
  (lambda (x k d) (if (jolt-truthy? (proxy-call x "containsKey" k)) (proxy-call x "get" k) d)))
(register-contains-arm! (lambda (x) (memq (proxy-coll-kind x) '(map set)))
  (lambda (x k) (jolt-truthy? (proxy-call x (if (eq? (proxy-coll-kind x) 'map) "containsKey" "contains") k))))

;; A base whose own methods call one a subclass overrides — LinkedHashMap's put
;; asking removeEldestEntry — is told which proxy stands over it, so that call
;; reaches the override as the JVM's virtual dispatch would. tag -> (base proxy)
(define proxy-attach-hooks '())
(define (proxy-attach-hook! tag f) (set! proxy-attach-hooks (cons (cons tag f) proxy-attach-hooks)))
(define (proxy-attach! base p)
  (when (jhost? base)
    (let ((h (assoc (jhost-tag base) proxy-attach-hooks)))
      (when h ((cdr h) base p)))))

;; ---- an Iterator whose remove writes through ---------------------------------------
;; (make-jutil-iterator tag items remove! [project]): walks ITEMS, a list taken
;; when the iterator is made; next() answers (project item) and remove() hands
;; the last item to REMOVE!, once — a second remove, or one before any next, is
;; the JDK's IllegalStateException. An item is whatever identifies an element to
;; its collection (an EnumMap's slot index, say). Each tag is one JDK iterator
;; class (classes follow tags); register-jutil-iterator! declares one.
;; State: #(remaining last remove! project)
(define jutil-iterator-none (list 'none))
(define (make-jutil-iterator tag items remove! . project)
  (make-jhost tag (vector items jutil-iterator-none remove!
                          (if (pair? project) (car project) (lambda (x) x)))))
(define jutil-iterator-methods
  (list (cons "hasNext" (lambda (self) (pair? (vector-ref (jhost-state self) 0))))
        (cons "next" (lambda (self)
                       (let* ((st (jhost-state self)) (xs (vector-ref st 0)))
                         (when (null? xs) (throw-jvm 'NoSuchElementException jolt-nil))
                         (vector-set! st 0 (cdr xs))
                         (vector-set! st 1 (car xs))
                         ((vector-ref st 3) (car xs)))))
        (cons "remove" (lambda (self)
                         (let* ((st (jhost-state self)) (x (vector-ref st 1)))
                           (when (eq? x jutil-iterator-none) (throw-jvm 'IllegalStateException jolt-nil))
                           (vector-set! st 1 jutil-iterator-none)
                           ((vector-ref st 2) x)
                           jolt-nil)))
        (cons "forEachRemaining" (lambda (self f)
                                   (let* ((st (jhost-state self)) (xs (vector-ref st 0)))
                                     (vector-set! st 0 '())
                                     (unless (null? xs) (vector-set! st 1 (list-ref xs (fx- (length xs) 1))))
                                     (for-each (lambda (x) (jolt-fi-call f "accept" ((vector-ref st 3) x))) xs)
                                     jolt-nil)))))
(define jutil-iterator-tags '())
(define (jutil-iterator? x) (and (jhost? x) (member (jhost-tag x) jutil-iterator-tags) #t))
(define (register-jutil-iterator! tag)
  (set! jutil-iterator-tags (cons tag jutil-iterator-tags))
  (register-host-methods! tag jutil-iterator-methods))
;; (iterator-seq it) / (seq it): what the iterator has not handed out yet
(register-seq-arm! jutil-iterator?
  (lambda (it)
    (let ((st (jhost-state it)))
      (if (null? (vector-ref st 0)) jolt-nil (list->cseq (map (vector-ref st 3) (vector-ref st 0)))))))

;; ---- java.util.Map$Entry host objects ----------------------------------------------
;; An entry a java.util shim hands out (TreeMap$Entry, SimpleImmutableEntry)
;; registers its tag with a procedure answering its (key . value). That is what
;; makes it a Map.Entry to the rest of the runtime: map-entry?, key / val, nth
;; and destructuring, count, conj onto a map (collections.ss jolt-host-entry),
;; = against another Map.Entry by key and value (and never against a vector:
;; a MapEntry's equiv wants a List), hash as Map.Entry.hashCode (k ^ v), and
;; str as k=v. pr is the #object form over that toString, as on the JVM.
(define (register-jutil-entry! tag kv)
  (hashtable-set! jutil-colls-tbl tag (vector 'entry kv))
  (register-host-methods! tag
    (list (cons "getKey" (lambda (self) (car (kv self))))
          (cons "getValue" (lambda (self) (cdr (kv self))))
          (cons "toString" (lambda (self) (jutil-entry-string-of self)))
          (cons "equals" (lambda (self o) (jutil-entry-equals? self o)))
          (cons "hashCode" (lambda (self) (jutil-entry-hash self))))))
(define (jutil-entry? x) (eq? (jutil-any-kind x) 'entry))
(define (jutil-entry-kv x)
  (let ((e (jutil-tag-entry x)))
    (and e (eq? (vector-ref e 0) 'entry) ((vector-ref e 1) x))))
(set! jolt-host-entry jutil-entry-kv)
(define (jutil-entry-string-of x)
  (let ((kv (jutil-entry-kv x)))
    (string-append (jutil-elem-string x (car kv)) "=" (jutil-elem-string x (cdr kv)))))
(define (jutil-entry-equals? a b)
  (let ((ka (jutil-entry-kv a)) (kb (jutil-entry-kv b)))
    (and ka kb (jolt=2 (car ka) (car kb)) (jolt=2 (cdr ka) (cdr kb)) #t)))
(define (jutil-entry-hash x)
  (let ((kv (jutil-entry-kv x)))
    (i32 (bitwise-xor (jolt-java-hashcode (car kv)) (jolt-java-hashcode (cdr kv))))))
(register-count-arm! jutil-entry? (lambda (x) 2))
;; map-entry? is (instance? java.util.Map$Entry x) on the JVM
(def-var! "clojure.core" "map-entry?"
  (lambda (x) (or (jolt-map-entry? x) (jutil-entry? x))))

;; ---- host Comparator objects -------------------------------------------------------
;; The comparator seam (natives-seq.ss jolt-comparator-fn) asks whether a value
;; is a shim object whose tag registers a `compare` method — a Comparator held
;; by the host (String/CASE_INSENSITIVE_ORDER, Comparator/reverseOrder) rather
;; than by a deftype/reify. Both targets carry jhost, so both answer it.
(set! jhost-compare-method?
  (lambda (x)
    (and (jhost? x) (host-method-ref (jhost-tag x) "compare") #t)))
