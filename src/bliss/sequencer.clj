(ns
  ^{:doc    "A flexible functional sequencer."
    :author "Kristoffer Ström"}
  bliss.sequencer
  (:use     bliss.helpers
            clojure.walk)
  (:require [clojure.set :as set]))

;; dynamic process variable

(def ^:dynamic *process*)

;; sequencing

(defn bundle? [object]
  (let [result (and (seq? object)
                    (every? coll? object))]
    (if result
      true
      (do
        (println "not a bundle: " object)
        false))))

(defn bundle [event]
  (assert (bundle? event))
  event)

(defn eventlist [data]
  (if (seq? data)
    (flatten data)
    (list data)))

(defn ass [args bundle]
  (map #(merge % (apply hash-map args))
       bundle))  

(defn ass [& kvs-and-bundle]
  (assert (odd? (count kvs-and-bundle)) "ass needs an odd number of arguments")
  (mapcat (fn [event]
         (map (fn [[key value]]                  
                (if (fn? value)
                  (if (key event)
                    (assoc event key (value (key event)))
                    event) ; pass through
                  (assoc event key value)))
              (partition 2 kvs-and-bundle)))
       (last kvs-and-bundle)))

(defn off [amount bundle]
  ;; (assert (bundle? bundle))
  (map #(assoc % :time (+ (get-time %) amount)) bundle))

(defn bundle-length [bundle]
  ;; (assert (bundle? bundle))
  (if (empty? bundle)
    0
    (apply max (map get-end bundle))))

;; tools

(defn scale-bundle [amount bundle]
  ;; (assert (bundle? bundle))
  (map (fn [map]
         (assoc map
           :time (* (get-time map) amount)
           :len  (* (get-len map) amount)))
       bundle))

;; variables

(defn set-var [var value]
  (list {:type  :variable
         :var   var
         :len   0
         :value value}))

(defn read-var [var & [default]]
  (loop [history (seq (:history *process*))]
    (if history
      (if (= (:type (first history)) :variable)
        (:value (first history))
        (recur (next history)))
      default)))

;; slightly contrieved use of eval here, because we need
;; to make sure that the pick happens at runtime
;; but still not have to evaluate all the alternatives

(defmacro oneof [& list]
  `(eval (nth '~list (rand-int (count '~list)))))

(defmacro oneafter [& list]
  `(eval (nth '~list (mod (read-var :measure 0)
                          (count '~list)))))

(defmacro oneafter-nr [bindings]
  (reduce + (map first (partition 2 ~bindings))))
  ;; `(eval (nth '~list (mod (read-var :measure 0)
  ;;                         (count '~list))))  

(defmacro oneseq [number & list]
  `(eval (nth '~list (mod ~number
                          (count '~list)))))

(defn end-of [num]
  (= (mod (read-var :measure 0) num)
     (dec num)))

(defn start-of [num]
  (= (mod (read-var :measure 0) num) 0))

(defn wrap-bundle [bundle]
  (fn [& kvs]
    (if (empty? kvs)
      bundle
      (map #(apply assoc % kvs) bundle))))

(defn pause [& args]
  (list
   (into (apply hash-map args)
         {:type :pause})))

(defn cc [& bundlelist]
  (loop [collect    '()
         offset     0
         bundle-seq (seq bundlelist)]
    (if bundle-seq
      (recur (concat collect (off offset (first bundle-seq)))
             (+ offset (or (bundle-length (first bundle-seq))
                           DEFAULT-LEN))
             (next bundle-seq))
      collect)))

(defn jn [& bundlelist]
  (apply concat bundlelist))

(defn n-template [fn args body]
  (let [[symbol sequence] args]
    `(let [body-fn# (fn [~symbol] ~@body)]
       (apply ~fn (map body-fn# ~sequence)))))

(defmacro cc-n [args & body]
  (n-template cc args body))

(defmacro jn-n [args & body]
  (n-template jn args body))

;; trimming

(defn trim [length bundle]
  (seq
   (reduce (fn [keep new]
             (if (>= (or (:time new) 0) length)
               keep
               (conj keep
                     (if (> (+ (or (:time new) 0) (:len new)) length)
                       (assoc new :len (- length (or (:time new) 0)))
                       new))))
           '[]
           bundle)))

(defmacro tr [length & body]
  `(trim ~length
         (loop [collect# '()]
           (if (< (bundle-length collect#) ~length)
             (recur (cc* collect# (eval '(cc ~@body))))
             collect#))))

;; measures

(defn count-measures [fun]
  (fn []
    (jn
     (set-var :measure (inc (read-var :measure 0)))
     (fun))))

(defmacro defcomb [name bindings & body]
  (let [bind-parts    (partition 2 bindings)
        replace-map   (into {}
                            (map (fn [[symbol default]]
                                   (vector symbol
                                           `(or ~symbol ~default)))
                                 bind-parts))
        body-defaults (postwalk-replace replace-map body)]
    `(defn ~name [& fn-args#]
       (let [{:keys ~(vec (map first bind-parts))} (apply hash-map fn-args#)]         
         ~@body-defaults))))
