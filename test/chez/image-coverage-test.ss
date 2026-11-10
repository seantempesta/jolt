;; Every java.util collection shim travels in a state image, or is refused by
;; name. A shim whose state an image cannot write as it is (a jolt-keyed
;; hashtable, a node chain, a comparator procedure) needs a codec
;; (java/image-codecs.ss); one that is a view or an entry of another collection
;; is refused. Without this, a new shim registered with the java.util registry
;; (jutil-colls.ss) would fail an image dump with "cannot write #<hashtable> at
;; state -> 0", naming neither the class nor what to do.
;;
;; What this pins:
;;   - every tag in the java.util registry has a SAMPLE below that round-trips
;;     (same class, same contents in order), or a REFUSED sample whose dump
;;     raises naming the class
;;   - every sample's value really carries the tag it is listed under, so the
;;     tables cannot drift from the shims
;;   - every codec and refusal registered names a tag listed here
;;   chez --script test/chez/image-coverage-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (ev s) (jolt-compile-eval (string-append "(do " s ")") "user"))
(define tmp (string-append (host-temp-dir) "/jolt-image-coverage-" (number->string (get-process-id)) ".jimg"))

(ev (string-append
  "(defn image-content [v]"
  "  (cond (instance? java.util.Map v) [:map (mapv (fn [e] [(key e) (val e)]) v)]"
  "        (instance? java.util.Collection v) [:coll (vec v)]"
  "        (instance? java.util.Map$Entry v) [:entry (key v) (val v)]"
  "        :else (str v)))"
  "(defn image-round-trips? [v]"
  "  (jolt.host/image-write! \"" tmp "\" v)"
  "  (let [r (jolt.host/image-read \"" tmp "\")]"
  "    (and (= (class v) (class r)) (= (image-content v) (image-content r)))))"
  "(defn image-refuses? [v]"
  "  (try (jolt.host/image-write! \"" tmp "\" v) false"
  "       (catch Exception e (clojure.string/includes? (ex-message e) (.getName (class v))))))"))

;; tag -> a Clojure expression whose value carries the tag and must round-trip
(define samples
  '(("arraylist" . "(java.util.ArrayList. [1 :a \"s\"])")
    ("arrays-aslist" . "(java.util.Arrays/asList (object-array [1 2]))")
    ("immutable-list" . "(.toList (.stream (java.util.ArrayList. [1 2 3])))")
    ("linkedlist" . "(java.util.LinkedList. [3 1 2])")
    ("arraydeque" . "(java.util.ArrayDeque. [1 2 3])")
    ("abq" . "(doto (java.util.concurrent.ArrayBlockingQueue. 4) (.add 1) (.add 2))")
    ("lbq" . "(doto (java.util.concurrent.LinkedBlockingQueue.) (.add :x))")
    ("hashmap" . "(doto (java.util.HashMap.) (.put :b 1) (.put \"a\" [2]) (.put nil 3))")
    ("properties" . "(doto (java.util.Properties. (doto (java.util.Properties.) (.setProperty \"d\" \"0\"))) (.setProperty \"a\" \"1\"))")
    ("hashset" . "(java.util.HashSet. [3 1 :k])")
    ("linkedhashmap" . "(doto (java.util.LinkedHashMap. 16 0.75 true) (.put :b 1) (.put :a 2) (.get :b))")
    ("linkedhashset" . "(java.util.LinkedHashSet. [3 1 2])")
    ("treemap" . "(doto (java.util.TreeMap. (java.util.Collections/reverseOrder)) (.put 1 :a) (.put 2 :b))")
    ("treeset" . "(java.util.TreeSet. [3 1 2])")
    ("enum-set-regular" . "(java.util.EnumSet/of java.util.concurrent.TimeUnit/DAYS java.util.concurrent.TimeUnit/SECONDS)")
    ("enum-map" . "(doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/HOURS [1]))")
    ("immutable-entry" . "(.firstEntry (java.util.TreeMap. {:k 1}))")
    ("lhm-snapshot-entry" . "(.firstEntry (doto (java.util.LinkedHashMap.) (.put :a 1)))")))
;; a JumboEnumSet needs an enum of more than 64 constants, which the host has
;; none of; its codec is the RegularEnumSet's (one registration, both tags)
(define same-codec '(("enum-set-jumbo" . "enum-set-regular")))

;; tag -> a Clojure expression whose value the image refuses, naming its class
(define refused
  '(("lhm-keyset" . "(.keySet (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-values" . "(.values (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-entryset" . "(.entrySet (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-reversed" . "(.reversed (doto (java.util.LinkedHashMap.) (.put :a 1)))")
    ("lhm-entry" . "(first (.entrySet (doto (java.util.LinkedHashMap.) (.put :a 1))))")
    ("lhs-reversed" . "(.reversed (java.util.LinkedHashSet. [1]))")
    ("enum-map-keyset" . "(.keySet (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1)))")
    ("enum-map-values" . "(.values (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1)))")
    ("enum-map-entryset" . "(.entrySet (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1)))")
    ("enum-map-entry" . "(first (.entrySet (doto (java.util.EnumMap. java.util.concurrent.TimeUnit) (.put java.util.concurrent.TimeUnit/DAYS 1))))")
    ("treemap-asc-sub" . "(.headMap (java.util.TreeMap. {1 1 2 2}) 2)")
    ("treemap-desc-sub" . "(.descendingMap (java.util.TreeMap. {1 1}))")
    ("treemap-keyset" . "(.keySet (java.util.TreeMap. {1 1}))")
    ("treemap-values" . "(.values (java.util.TreeMap. {1 1}))")
    ("treemap-submap-values" . "(.values (.headMap (java.util.TreeMap. {1 1 2 2}) 2))")
    ("treemap-entryset" . "(.entrySet (java.util.TreeMap. {1 1}))")
    ("treemap-asc-entryset" . "(.entrySet (.headMap (java.util.TreeMap. {1 1 2 2}) 2))")
    ("treemap-desc-entryset" . "(.entrySet (.descendingMap (java.util.TreeMap. {1 1})))")
    ("treemap-entry" . "(first (.entrySet (java.util.TreeMap. {1 1})))")))

(define (tag-of v) (and (jhost? v) (jhost-tag v)))

;; every collection tag is covered
(let-values (((ks vs) (hashtable-entries jutil-colls-tbl)))
  (vector-for-each
    (lambda (t)
      (ok (format "java.util shim tag ~a has no image sample: add one to image-coverage-test.ss that round-trips (with a codec in java/image-codecs.ss if its state cannot travel), or a refused sample" t)
          (or (assoc t samples) (assoc t refused) (assoc t same-codec))))
    ks))

;; the samples round-trip
(for-each
  (lambda (p)
    (let ((v (ev (cdr p))))
      (ok (format "sample for ~a carries that tag (got ~a)" (car p) (tag-of v)) (equal? (tag-of v) (car p)))
      (ok (format "~a round-trips through a state image" (car p))
          (guard (e (#t (printf "  ~a: ~a\n" (car p) (guard (_ (#t e)) (jolt-final-str (jolt-invoke (var-deref "clojure.core" "ex-message") e)))) #f))
            (jolt-truthy? (jolt-invoke (var-deref "user" "image-round-trips?") v))))))
  samples)

;; the refused samples are refused, by class name
(for-each
  (lambda (p)
    (let ((v (ev (cdr p))))
      (ok (format "refused sample for ~a carries that tag (got ~a)" (car p) (tag-of v)) (equal? (tag-of v) (car p)))
      (ok (format "~a is refused naming its class" (car p))
          (jolt-truthy? (jolt-invoke (var-deref "user" "image-refuses?") v)))))
  refused)

;; every codec and refusal registered is one this file knows about
(let-values (((ks vs) (hashtable-entries image-codec-by-tag)))
  (vector-for-each
    (lambda (t) (ok (format "codec tag ~a has a sample here" t)
                    (or (assoc t samples) (assoc t same-codec)
                        (member t '("string-builder" "string-buffer")))))
    ks))
(let-values (((ks vs) (hashtable-entries image-refusal-by-tag)))
  (ok "every refused collection view or entry has a refused sample"
      (for-all (lambda (t)
                 (or (assoc t refused) (not (hashtable-ref jutil-colls-tbl t #f))))
               (vector->list ks))))

;; the values that are not collections but carry host state
(for-each
  (lambda (s)
    (ok (format "~a round-trips" s)
        (jolt-truthy? (jolt-invoke (var-deref "user" "image-round-trips?") (ev s)))))
  '("(StringBuilder. \"ab\")" "(StringBuffer. \"cd\")"))
(ok "a thread is refused naming its class"
    (jolt-truthy? (jolt-invoke (var-deref "user" "image-refuses?") (ev "(Thread. (fn []))"))))
(ok "a collection holding itself restores as one"
    (begin
      (ev (string-append "(let [m (java.util.HashMap.)] (.put m :me m) (jolt.host/image-write! \"" tmp "\" m))"))
      (jolt-truthy? (ev (string-append "(let [r (jolt.host/image-read \"" tmp "\")] (identical? r (.get r :me)))")))))

(when (file-exists? tmp) (delete-file tmp))
(printf "image coverage gate: ~a/~a passed\n" (- total fails) total)
(exit (if (zero? fails) 0 1))
