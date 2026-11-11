;; A cell written to a state image while it was being forced holds a tail-claim in its
;; tail word. Runtimes before the claim was made once an owner wrote it as
;; jolt-tail-claim-v1, a uid the current runtime does not define, so the restore must
;; put the pending word it carries back (state-image.ss image-legacy-tail-claim?): the
;; cell is then pending, and forcing it gives its value. Without that arm the v1 record
;; is not a tail-claim, reads as a realized tail, and comes back as the cell's rest.
(ns image-legacy-claim-test (:require [jolt.scheme :as scm]))

(scm/eval-string "
(define ilc-make-v1
  (record-constructor
    (make-record-constructor-descriptor
      (make-record-type-descriptor 'tail-claim #f 'jolt-tail-claim-v1 #f #f
        '#((immutable tail) (immutable owner) (immutable token)))
      #f #f)))
(define (ilc-claim-tail! c) (cseq-tail-set! c (ilc-make-v1 (cseq-tail c) 7 (list 'forcing))) c)
(define (ilc-write path v) (jolt-image-write! path v))
(define (ilc-read path) (jolt-image-read path))")

(def tail (lazy-seq (list 2 3)))
(doall tail)
(def cell ((scm/proc "ilc-claim-tail!") (cons 1 tail)))
(def path (str (System/getProperty "java.io.tmpdir") "/image-legacy-claim-" (System/currentTimeMillis) ".img"))
((scm/proc "ilc-write") path cell)
(def got (try (vec ((scm/proc "ilc-read") path)) (catch Throwable e [:threw (ex-message e)])))
(.delete (java.io.File. path))
(println (if (= got [1 2 3])
           "image-legacy-claim: ok"
           (str "image-legacy-claim: FAIL " (pr-str got) " expected [1 2 3]")))
