;; Literal integers past the fixnum range, negative ones above all, come back
;; exactly in a built binary as they do from source. Chez's vfasl writer once
;; wrote a negative bignum's two's-complement bits under a sign-magnitude header,
;; so -4658895280553007687 loaded as -13787848793156543929 in every jolt build
;; (test.check's SplitMix constants among them), while jolt run was right.
(ns blapp.main)

(def consts [-4658895280553007687 4658895280553007687 -9223372036854775808
             -123456789012345678901234567890N -1/123456789012345678901234567890])

(defn -main [& _]
  (println "BIGNUM-LITERALS" consts (unchecked-multiply 1234567 -4658895280553007687)))
