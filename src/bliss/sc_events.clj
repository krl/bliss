(ns bliss.sc-events
  (:use overtone.live
        bliss.sequencer))

(def get-sample
  (memoize (fn [path]
             (load-sample path))))

(definst buf-player [buf 0 rate 1 amp 1 pan 0]
  (pan2
   (* amp (play-buf 1 buf rate 1.0 0.0 0.0 :action FREE))   
   pan))

(defn inst-bundle [inst default-map]
  (list
   (merge {:action (fn [self time]
                     (at time
                         ;; apply with the key-value pairs where (number? value), spliced
                         ;; (println "inst:" inst)
                         (apply inst (apply concat
                                            (filter #(number? (second %)) self)))))
           :type :sc-event
           :len 1}
          default-map)))

(defmacro def-inst-bundle [i-name parameters & inst-form]
  (let [default-map (apply hash-map (map #(if (symbol? %) (keyword %) %) parameters))
        proxy-name (symbol (str (name i-name) "-proxy"))]
    `(do
       (definst ~proxy-name ~parameters ~@inst-form)
       (defn ~i-name [& kvs#]
         (inst-bundle ~proxy-name (merge ~default-map (apply hash-map kvs#)))))))

(defn sample-bundle [path & kvs]
  (fn [& kvs-override]
    (inst-bundle buf-player
                 (merge {:buf (:id (get-sample path))}
                        (apply hash-map kvs)
                        (apply hash-map kvs-override)))))
