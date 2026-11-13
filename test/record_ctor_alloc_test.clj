;; A record built by its class constructor, (T. a b c), allocates what the
;; record is and nothing else: the same bytes as its factory (->T a b c), the
;; type's positional constructor called at its arity. Before this
;; gate, (T. …) beyond six fields, or of a type the compiling unit did not
;; define, went through host-new, which consed its arguments into a list and
;; applied the constructor to it (16 bytes a field), and a type of more than
;; eight fields was built by a rest list, a field vector, vector->list and apply
;; (a ten-field record: 464 bytes where its vector and record are 144). A sorted
;; set's seq (persistent-sorted-set's Seq, ten fields) is built once per tree
;; level per seek and once per element stepped, so every index read paid it.
;;
;; Counted with the calling thread's own allocation (Chez
;; thread_bytes_allocated), which a collection never lowers and no other
;; thread adds to: each arm builds N records into one array and reads the
;; difference, so the array's own bytes are outside both readings.
;; And a call of five to eight arguments, or of a protocol method of four, allocates nothing.
;; Run: target/release/jolt run test/record_ctor_alloc_test.clj
(require '[jolt.scheme :as scm])

(def thread-bytes
  (scm/eval-string "(foreign-procedure \"(cs)thread_bytes_allocated\" () uptr)"))

(defprotocol P4 (m4 [x a b c]))
(deftype T3 [a b c] P4 (m4 [_ x y z] x))
(deftype T10 [a b c d e f g h i j])
(deftype T12 [a b c d e f g h i j k l])

(def n 1000)
(def out (object-array n))

(defmacro bytes-per [form]
  `(do (dotimes [i# n] (aset out i# ~form))
       (let [b0# (thread-bytes)]
         (dotimes [i# n] (aset out i# ~form))
         (quot (- (thread-bytes) b0#) n))))

(defn- fail! [& parts]
  (println (apply str "FAIL record-ctor-alloc: " parts))
  (System/exit 1))

;; the constructor called through a value the compiler cannot see through: the
;; runtime's host-new and the type's own constructor procedure
(def ^:dynamic *x* 1)

(let [arms [["3 fields" (bytes-per (T3. *x* 2 3)) (bytes-per (->T3 *x* 2 3))]
            ["10 fields" (bytes-per (T10. *x* 2 3 4 5 6 7 8 9 10)) (bytes-per (->T10 *x* 2 3 4 5 6 7 8 9 10))]
            ["12 fields" (bytes-per (T12. *x* 2 3 4 5 6 7 8 9 10 11 12)) (bytes-per (->T12 *x* 2 3 4 5 6 7 8 9 10 11 12))]]
      ;; a record of k fields is at most a header, its descriptor, extension and hash
      ;; slots and k fields, or past eight a spill record and a k-slot vector:
      ;; 8 bytes a word, rounded to 16
      bound (fn [k] (if (<= k 8) (* 16 (quot (+ (* 8 (+ 4 k)) 15) 16))
                        (+ 48 (* 16 (quot (+ (* 8 (+ 1 k)) 15) 16)))))]
  (doseq [[label ctor factory] arms]
    (println (str "record-ctor-alloc " label ": (T. ...) " ctor " B, (->T ...) " factory " B")))
  (doseq [[[label ctor factory] k] (map vector arms [3 10 12])]
    (when (> ctor (bound k)) (fail! label ": (T. ...) allocated " ctor " B, over the record's " (bound k)))
    ;; the factory is the ctor itself (deftype's (def ->T T)); called with more than eight
    ;; arguments it is a call jolt-invoke takes variadic, whose rest list is the call's, not the record's
    (when (and (<= k 8) (> factory (bound k)))
      (fail! label ": (->T ...) allocated " factory " B, over the record's " (bound k))))
  ;; a call of five to eight arguments: past four the back end called variadic
  ;; jolt-invoke, which consed them to apply the fn (16 B an argument per call)
  (let [f5 (fn [a b c d e] a) f8 (fn [a b c d e f g h] a)
        c5 (quot (let [b0 (thread-bytes)] (dotimes [_ n] (f5 *x* 2 3 4 5)) (- (thread-bytes) b0)) n)
        c8 (quot (let [b0 (thread-bytes)] (dotimes [_ n] (f8 *x* 2 3 4 5 6 7 8)) (- (thread-bytes) b0)) n)]
    (println (str "record-ctor-alloc a call of 5 arguments " c5 " B, of 8 " c8 " B"))
    (when (or (pos? c5) (pos? c8)) (fail! "a call of 5 or 8 arguments allocated " c5 " / " c8 " B")))
  ;; a protocol method of four parameters: it went to variadic protocol-dispatch,
  ;; a vector of the extra args made a list and applied (208 B a call)
  (let [r (T3. 1 2 3)
        p4 (quot (let [b0 (thread-bytes)] (dotimes [_ n] (m4 r *x* 2 3)) (- (thread-bytes) b0)) n)]
    (println (str "record-ctor-alloc a 4-parameter protocol call " p4 " B"))
    (when (pos? p4) (fail! "a 4-parameter protocol call allocated " p4 " B")))
  (println "record-ctor-alloc OK"))
