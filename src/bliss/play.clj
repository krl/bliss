(ns
  ^{:doc    "A flexible functional sequencer."
    :author "Kristoffer Ström"}
  bliss.play
  (:use bliss.sequencer
        bliss.helpers))

(def PRESEQUENCE-LENGTH 2)
(def PRE-WRITE-TIME     0.1)

(def playing*    (atom false))

;; sequencing

(defn- history-length [history]
  (get-end (first history)))

(defn- sort-bundle [bundle]
  (assert (bundle? bundle))
  (reverse (sort-by get-end bundle)))

(defn- increment-history [process]
  "uses the pool function to return an updated process hashmap"
  (let [new-history (concat (off (history-length (:history process))
                                 (sort-bundle
                                  (binding [*process* process]
                                    ((:pool-fn process)))))
                            (:history process))]
    (assoc process :history new-history)))

(defn- time-since-start [process]
  (/ (- (timestamp) (:start-time process)) 1000.0))

(defn- presequenced
  "The amount of time that is scheduled ahead of current timestamp"
  [process]
  (- (history-length (:history process)) (time-since-start process)))

(defn- write-events [process]
  "
write the events that start :last-write > event > (:last-write + PRE-WRITE-TIME)
returns the updated process
"
  (let [last-write (:last-write process)]
    (doseq [event (filter #(and (<= last-write
                                    (get-time %))
                                (<  (get-time %)
                                    (+ last-write PRE-WRITE-TIME))
                                (:action %))
                          (:history process))]
      ((:action event) event (+ (:start-time process) (* (get-time event) 1000))))
    (assoc process :last-write (+ last-write PRE-WRITE-TIME))))

(defn- thread-function [process process*]
  (when @playing*
                                        ;(println "playing!")
    ;; update reference
    (swap! process* (fn [_] process))

    (cond
     ;; enough data already sequenced?
     (< (presequenced process) PRESEQUENCE-LENGTH)
     (do ;(println "data")
       (recur (increment-history process) process*))

     ;; write events to output
     (> (- (time-since-start process) (:last-write process) PRE-WRITE-TIME) (- (* PRE-WRITE-TIME 2)))
     (do ; (println "write")
       (recur (write-events process) process*))
     
     :else ; recur with identical data in a while
     (do ;; (println "sleep")
       (. Thread (sleep 10))
       (recur process process*)))))

(defn start-process-fn [pool-fn sync]
  "
starts a player-process using pool-fn, and returns a reference that is updated
with current state from the thread-function
"
  (swap! playing* (fn [_] true))
  (let [process* (atom nil)
        process  {:start-time  (+ (timestamp) 400)
                  :last-write  0
                  :playing*    playing*
                  :history     '()
                  :pool-fn     pool-fn}]
    (println "starting thread")
    (if sync
      (do
        (thread-function process process*)
        (println "thread stopped"))
      (future
        (thread-function process process*)
        (println "thread stopped")))
    process*))

(defmacro start-process [pool-fn]
  "provides a wrapper that allows redefining the top pool function on the fly"
  `(start-process-fn (count-measures #(~pool-fn)) false))

(defmacro start-process-sync [pool-fn]
  "provides a wrapper that allows redefining the top pool function on the fly"
  `(start-process-fn (count-measures #(~pool-fn)) true))

(defn stop-all-processes []
  (swap! playing* (fn [_] false)))

