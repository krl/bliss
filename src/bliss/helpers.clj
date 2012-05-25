(ns bliss.helpers
  (:import java.util.Date))

(def DEFAULT-LEN 1)

(defn timestamp []
  (.getTime (Date.)))

(defn get-len [event]
  (or (:len event) DEFAULT-LEN))

(defn get-time [event]
  (or (:time event) 0))

(defn get-end [event]
  (+ (get-len event)
     (get-time event)))

;;

(defmacro dbg [& body]
  `(let [result# ~@body]
     (println (quote ~@body) " => " result#)
     result#))
  

(defn range+ [& args]
  (map inc (apply range args)))

(defn rand-int+ [num]
  (inc (rand-int num)))

(defn rand-int-range [min max]
  (+ min (rand-int (- max min -1))))
