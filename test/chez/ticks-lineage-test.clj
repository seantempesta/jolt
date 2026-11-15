;; A measure counts the work of the fibers its body spawns (tests-4's case, task jolt.host/an-off-fiber-measure-counts-its-own-
;; lineage): a measure (jolt.host/ticks-of, and a jolt.host/ticks delta taken inside one, as seon.profile reads it) counted only
;; its own fiber's or thread's ticks, so a body that spawned a spinning go block and waited on it read 241 ticks for 5M of work
;; [M, 2026-11-15], and a test spinning in its own go block escaped its budget. A fiber spawned under an open measure charges
;; its dispatches to that measure and its enclosing ones; a stranger's fiber charges none (82 ticks around such a wait, already).
;;
;; Run: target/release/jolt run test/chez/ticks-lineage-test.clj   (prints TICKS-LINEAGE-TEST OK)
(ns ticks-lineage-test
  (:require [clojure.core.async :as a]))

(defn- fail! [& parts] (println (apply str "FAIL ticks-lineage: " parts)) (System/exit 1))
(defn spin [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc i)) acc)))
(def N 5000000)
(defn spawn-spin-and-wait [n] (let [c (a/chan)] (a/go (spin n) (a/>! c 1)) (a/<!! c)))

;; (b) a body's own spinner is its work, through ticks-of and through a ticks delta inside it
(let [t (jolt.host/ticks-of #(spawn-spin-and-wait N))]
  (when (< t N) (fail! "a measure missed its own go block's work: " t " ticks for " N)))
(let [d (volatile! nil)]
  (jolt.host/ticks-of (fn [] (let [k0 (jolt.host/ticks)] (spawn-spin-and-wait N) (vreset! d (- (jolt.host/ticks) k0)))))
  (when (< @d N) (fail! "a ticks delta inside a measure missed its go block's work: " @d " for " N)))

;; a stranger's spinner, started before the measure, stays out of it
(let [c (a/chan)]
  (a/go (spin N) (a/>! c 1))
  (let [t (jolt.host/ticks-of #(a/<!! c))]
    (when (> t (quot N 10)) (fail! "a stranger's work counted in a measure that only waited: " t))))

;; (c) nested: an outer measure counts its grandchild's work; an inner measure counts it once, and its own fiber once
(let [inner (promise)
      outer (jolt.host/ticks-of
              (fn [] (let [c (a/chan)]
                       (a/go (deliver inner (jolt.host/ticks-of #(spawn-spin-and-wait N))) (a/>! c 1))
                       (a/<!! c))))]
  (when (< @inner N) (fail! "an inner measure missed its child's work: " @inner))
  (when (> @inner (* 2 N)) (fail! "an inner measure counted its work twice: " @inner " for " N))
  (when (< outer N) (fail! "an outer measure missed its grandchild's work: " outer))
  (when (> outer (* 2 N)) (fail! "an outer measure counted its grandchild twice: " outer " for " N)))

(println "TICKS-LINEAGE-TEST OK")
(System/exit 0)
