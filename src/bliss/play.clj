(ns
  ^{:doc    "A flexible functional sequencer."
    :author "Kristoffer Ström"}
  bliss.play
  (:use bliss.sequencer
        bliss.helpers))

(def PRESEQUENCE-LENGTH 2)
(def PRE-WRITE-TIME     1)

(def playing* (atom '()))
(def thread*  (atom nil))

(alter-var-root #'*out* (constantly *out*))

;; sequencing

(defn- history-length [history]
  (get-end (first history)))

(defn- sort-bundle [bundle]
  ;; (assert (bundle? bundle))
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

(defn- write-events
  "write the events that start :last-write > event > (:last-write + PRE-WRITE-TIME)
returns the updated process"
    [process]

  (let [last-write (:last-write process)]
    (doseq [event (filter #(and (<= last-write
                                    (get-time %))
                                (<  (get-time %)
                                    (+ last-write PRE-WRITE-TIME))
                                (:action %))
                          (:history process))]
      ((:action event) event (+ (:start-time process) (* (get-time event) 1000))))
    (assoc process :last-write (+ last-write PRE-WRITE-TIME))))

(defn- update-process [process]
  ;; (println "updating process" (:pool-fn process))
  (cond
   ;; enough data already sequenced?
   (< (presequenced process) PRESEQUENCE-LENGTH)
   (recur (increment-history process))

   ;; write events to output
   (> (- (time-since-start process) (:last-write process) PRE-WRITE-TIME)
      (- (* PRE-WRITE-TIME 2)))
   (recur (write-events process))

   ;; return the same value
   :else process))   

(defn- thread-function []
  (when-not (empty? @playing*)
    (doall (swap! playing*
                  #(map update-process %)))
    (. Thread (sleep 100))
    (recur)))

(defn start-player-thread []
  (when-not @thread*
    (reset! thread*
            (.start (Thread. (fn []
                               (println "thread starting")
                               (thread-function)
                               (reset! thread* nil)
                               (println "thread stopped")))))))
  
(defn make-process [pool-fn]
  {:start-time  (+ (timestamp) 400)
   :last-write  0
   :history     '()
   ;; FIXME, find cleaner solution
   ;; this uses eval to always re-evaluate
   ;; the function reference, as to allow changing
   ;; the top level function while running
   :pool-fn     (fn [] (eval `(~pool-fn)))})

(defn set-processes [& processes]
  (reset! playing* (map make-process processes))
  (start-player-thread))

(defn stop-all-processes []
  (reset! playing* '()))
