;; put!/take! that cannot complete at once are waiters on the channel (async.ss, the
;; R3 alt-handler with the callback as its wake): no OS thread per pending op, FIFO
;; kept, each callback run exactly once, on a fiber. n <= 100.
(require '[clojure.core.async :as a])

(def failures (atom []))
(defn check-eq [what got want]
  (when-not (= got want) (swap! failures conj (str what ": got " (pr-str got) ", want " (pr-str want)))))
(defn nthreads [] (count (Thread/getAllStackTraces)))
(defn await-until [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop [] (cond (pred) true (> (System/currentTimeMillis) end) false :else (do (Thread/sleep 1) (recur))))))

(a/<!! (a/go 1))   ; the carriers are up before anything is counted
(def n 100)

;; 100 pending put! on an unbuffered channel
(let [c (a/chan) done (atom []) t0 (nthreads)]
  (dotimes [i n] (a/put! c i (fn [ok] (swap! done conj [i ok]))))
  (check-eq "100 pending put! add no thread" (- (nthreads) t0) 0)
  (check-eq "the pending puts are taken in the order they were made" (vec (repeatedly n #(a/<!! c))) (vec (range n)))
  (check-eq "every put! callback ran" (await-until 2000 #(= n (count @done))) true)
  (Thread/sleep 20)
  (check-eq "each put! callback ran once, with true" (sort-by first @done) (map #(vector % true) (range n)))
  (check-eq "no thread left" (- (nthreads) t0) 0))

;; 100 pending take!
(let [c (a/chan) got (atom []) t0 (nthreads)]
  (dotimes [i n] (a/take! c (fn [v] (swap! got conj [i v]))))
  (check-eq "100 pending take! add no thread" (- (nthreads) t0) 0)
  (dotimes [i n] (a/>!! c i))
  (check-eq "every take! callback ran" (await-until 2000 #(= n (count @got))) true)
  (Thread/sleep 20)
  (check-eq "the pending takes got the values in the order they were made, once each"
            (sort-by first @got) (map #(vector % %) (range n))))

;; close: a pending put! is answered false, a pending take! nil, once each
(let [c (a/chan) put-r (atom []) take-r (atom [])
      d (a/chan)]
  (a/put! c :x #(swap! put-r conj %))
  (a/take! d #(swap! take-r conj %))
  (a/close! c) (a/close! d)
  (await-until 2000 #(and (seq @put-r) (seq @take-r)))
  (Thread/sleep 20)
  (check-eq "a pending put! on a closed channel is answered false once" @put-r [false])
  (check-eq "a pending take! on a closed channel is answered nil once" @take-r [nil]))

;; mult with a slow tap: its deliveries wait as waiters, not threads
(let [src (a/chan) m (a/mult src) fast (a/chan 100) slow (a/chan) t0 (nthreads)]
  (a/tap m fast) (a/tap m slow)
  (a/>!! src 0)
  (check-eq "a mult delivery to a slow tap holds no thread" (- (nthreads) t0) 0)
  (check-eq "the slow tap gets the value when it reads" (a/<!! slow) 0)
  (check-eq "the fast tap got it too" (a/<!! fast) 0))

(if (empty? @failures)
  (do (println "ASYNC-CALLBACK-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f)) (flush) (System/exit 1)))
