(ns
  ^{:doc    "A flexible functional sequencer."
    :author "Kristoffer Ström"}
  bliss.sequencer
  (:use     bliss.helpers)
  (:require clojure.set))

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

(defn off [amount bundle]
  (assert (bundle? bundle))
  (map #(assoc % :time (+ (get-time %) amount)) bundle))

(defn bundle-length [bundle]
  (assert (bundle? bundle))
  (if (empty? bundle)
    0
    (apply max (map get-end bundle))))

;; tools

(defn scale-bundle [amount bundle]
  (assert (bundle? bundle))
  (map (fn [map]
         (assoc map
           :time (* (get-time map) amount)
           :len  (* (get-len map) amount)))
       bundle))

(defn trim [trim-length bundle]
  (loop [collect '()
         event-seq (seq bundle)]
    (if event-seq
      (let [event (first event-seq)]
        (if (< (get-time event) trim-length)
          (recur (conj collect (assoc event :len (min (get-end event) (- trim-length (get-time event)))))
                 (next event-seq))
          (recur collect nil)))
      (reverse collect))))

(defn jn [& bundlelist]
  (apply concat bundlelist))

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

(defn apply-ass [event kvs]
  (if kvs
    (recur (let [[key value] (first kvs)]
             (if (fn? value)
               (assoc event key (value event))
               (assoc event key value)))
           (next kvs))
    event))

(defn ass [& kvs-and-bundle]
  (assert (odd? (count kvs-and-bundle)))
  (let [bundle (last    kvs-and-bundle)
        kvs    (apply hash-map (butlast kvs-and-bundle))]
    (assert (bundle? bundle))   
    (map #(apply-ass % kvs) bundle)))

;; variables

(defn set-var [var value]
  (list {:type  :variable
         :var   var
         :value value}))

(defn read-var [var & [default]]
  (loop [history (seq (:history *process*))]
    (if history
      (if (= (:type (first history)) :variable)
        (:value (first history))
        (recur (next history)))
      default)))

;; small things

(defn rand+1-int [num]
  (+ 1 (rand-int num)))

(defn rand-int-range [min max]
  (+ min (rand-int (- max min -1))))


;; slightyl contrieved form here, because we need to make sure that the pick happens at runtime
;; but still not have to evaluate all the alternatives
(defmacro oneof [& list]
  `(eval (nth '~list (rand-int (count '~list)))))

(defmacro oneafter [& list]
  `(eval (nth '~list (mod (read-var :measure 0)
                          (count '~list)))))

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

(defn count-measures [fun]
  (fn []
    (jn
     (set-var :measure (inc (read-var :measure 0)))
     (fun))))

;; new form  

(defn- combine-var [combinator var over body]
  `(apply ~combinator (map (fn [val#]
                            (let [~var val#]
                              (~combinator ~@body)))
                          ~over)))

(defn- combine [combinator over body]
  `(apply ~combinator (map (fn [_#]
                             (~combinator ~@body))
                           ~over)))

;; reach out!

(defmacro cc-n [nr & body]
  (combine cc `(range ~nr) body))

(defmacro cc-nv [var nr & body]
  (combine-var cc var (map inc (range nr)) body))

(defmacro cc-nv0 [var nr & body]
  (combine-var cc var `(range ~nr) body))

(defmacro cc-no [var over & body]
  (combine-var cc var over body))

(defmacro jn-n [nr & body]
  (combine jn `(range ~nr) body))

(defmacro jn-nv [var nr & body]
  (combine-var jn var `(map inc (range ~nr)) body))

(defmacro jn-nv0 [var nr & body]
  (combine-var jn var `(range ~nr) body))

;; (defn- trim-loop [length body]
  
;; (defmacro trim [length & body]
;;   (trim-loop length body))