;; An uncaught throw of a go block or a fiber body reaches the default uncaught-exception
;; handler, as a thread's does: a program that set one is told of every body that died,
;; and stderr is only where they go when it set none. Prints its name, then ok or FAIL
;; with what the handler saw.
;;
;; The handler is the program's code, so it runs holding no lock of the runtime's: a
;; transducer's completion that throws on close! is reported after the channel's mutex
;; is let go. Its handler parks (a take of `parked`, which only the closer fills, after
;; its close! returns); run under the mutex, on a fiber, that take raises (a fiber
;; cannot park holding a counted lock), the report is lost, and the set below misses it.
(require '[clojure.core.async :as a])

(def seen (atom []))
(def parked (a/chan))
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ _ ex]
      (when (= "completion" (ex-message ex)) (a/<!! parked))
      (swap! seen conj (ex-message ex)))))

(defn settled [n] (loop [i 0] (when (and (< (count @seen) n) (< i 2000)) (Thread/sleep 1) (recur (inc i)))))

(a/<!! (a/go (throw (ex-info "go" {}))))
(.join (Thread/startVirtualThread #(throw (ex-info "fiber" {}))))
(.join (doto (Thread. #(throw (ex-info "thread" {}))) .start))
(a/<!! (binding [a/*go-backend* :fiber] (a/go (throw (ex-info "go on a fiber" {})))))
(.join (Thread/startVirtualThread
         #(let [xf (fn [rf] (fn ([] (rf)) ([_] (throw (ex-info "completion" {}))) ([acc x] (rf acc x))))]
            (a/close! (a/chan 1 xf))
            (a/put! parked :go))))
(settled 5)
(println "fiber-uncaught:" (if (= #{"go" "fiber" "thread" "go on a fiber" "completion"} (set @seen)) "ok" (str "FAIL " (pr-str @seen))))
