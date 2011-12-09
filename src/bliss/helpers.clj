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
  
 