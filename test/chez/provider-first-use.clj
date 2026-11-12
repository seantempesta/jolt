;; A class a library provides (RFC 0014: jolt.socket's java.net classes, jolt.time.base's
;; java.time ones) is found from any fiber or thread, however many reach it first at once.
;; Its provider loads lazily on the first reference that misses, once; a reference from
;; another context while that load runs used to miss too ("No dependency provides
;; java.net.ServerSocket"), because the claim settles before the load. It now waits for
;; the load, as a require waits for another context's load of the same namespace.
;; And a first load that run-interruptible's budget ends (a jump out, never a raise) is
;; loaded again by the next reference: its claim stayed taken, and every later reference
;; from the thread said "No dependency provides java.net.Socket" (Seon, 2026-11-13: a
;; test that spent its budget inside jolt.socket's load left the next 18 without sockets).
;; Each case is its own process (provider-first-use.sh), since a provider loads once.
(require '[clojure.core.async :as a])
(def mode (first *command-line-args*))
(defn round-trip []
  (try (let [ss (java.net.ServerSocket. 0) port (.getLocalPort ss)
             t (future (try (.close (.accept ss)) (catch Throwable _ nil)))]
         (with-open [_ (java.net.Socket. "127.0.0.1" (int port))] (deref t 5000 nil) (.close ss) :ok))
       (catch Throwable e (str "ERR " (ex-message e)))))
(defn a-date [] (try (str (java.time.LocalDate/of 2026 11 11)) (catch Throwable e (str "ERR " (ex-message e)))))
(defn on-fibers [n f] (mapv a/<!! (mapv (fn [_] (a/go (f))) (range n))))
(defn on-futures [n f] (mapv deref (mapv (fn [_] (future (f))) (range n))))
;; the first reference under a budget far below jolt.socket's load (:spent when the budget
;; ended it, which the case needs), then a round trip on the same thread
(defn after-a-spent-budget []
  [(try (jolt.host/run-interruptible (jolt.host/make-interrupt)
                                     #(java.net.InetSocketAddress. "127.0.0.1" (int 1)) 20000)
        :finished
        (catch Throwable e (if (:jolt/ticks-spent (ex-data e)) :ok (str "ERR " (ex-message e)))))
   (round-trip)])
(def got
  (case mode
    "fiber" [(a/<!! (a/go (round-trip)))]
    "future" [@(future (round-trip))]
    "thread" [(let [p (promise)] (.start (Thread. #(deliver p (round-trip)))) @p)]
    "main-then-fiber" [(round-trip) (a/<!! (a/go (round-trip)))]
    "fibers" (on-fibers 8 round-trip)
    "futures" (on-futures 8 round-trip)
    "fibers-and-futures" (let [fs (mapv (fn [_] (a/go (round-trip))) (range 4))
                               ts (mapv (fn [_] (future (round-trip))) (range 4))]
                           (into (mapv a/<!! fs) (map deref ts)))
    "statics-on-fibers" (on-fibers 8 a-date)
    "after-a-spent-budget" (after-a-spent-budget)))
(def want (if (= mode "statics-on-fibers") "2026-11-11" :ok))
(if (every? #(= want %) got)
  (println (str "provider-first-use " mode ": OK " (count got)))
  (do (println (str "provider-first-use " mode ": FAIL " (pr-str (remove #(= want %) got))))
      (System/exit 1)))
(System/exit 0)
