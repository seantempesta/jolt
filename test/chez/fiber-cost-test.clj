;; A fiber's cost is charged per dispatch (fibers.ss jolt-fiber-drain!): its ticks, the bytes it
;; allocated and the CPU time it took (jolt.host/fiber-cost). CPU time counts what ticks cannot: a
;; fiber that spends its dispatch in one C call (arc4random_buf over 64 MB) shows CPU time far above
;; its ticks, and a fiber that allocates about 8 MB is charged them. A root step that spent half its
;; samples in dlsym showed none of it in ticks (2026-11-15).
(require '[jolt.host :as h] '[jolt.scheme :as scm])

(def fill! (scm/eval-string
             "(let ([f (foreign-procedure \"arc4random_buf\" (u8* size_t) void)])
                (lambda (bv) (f bv (bytevector-length bv))))"))

(defn on-a-fiber [body]
  (let [p (promise)]
    (h/fiber-spawn (fn [] (let [r (body)] (deliver p [(h/current-fiber) r]))))
    (let [[f r] (deref p 30000 nil)]
      (Thread/sleep 50)
      [(h/fiber-cost f) r])))

(defn check [what ok? cost]
  (if ok?
    (println "fiber-cost:" what "OK" cost)
    (do (println "fiber-cost:" what "FAIL" cost) (System/exit 1))))

(let [bv (scm/eval-string "(make-bytevector (* 64 1024 1024) 0)")
      [[ticks _ cpu] _] (on-a-fiber #(fill! bv))]
  (check "C time is CPU time, not ticks" (and (> cpu 5000000) (> cpu (* 100 ticks))) {:ticks ticks :cpu-ns cpu}))

(let [[[_ bytes _] n] (on-a-fiber #(count (vec (range 1000000))))]
  (check "allocation is charged to the fiber" (> bytes 8000000) {:bytes bytes :n n}))
