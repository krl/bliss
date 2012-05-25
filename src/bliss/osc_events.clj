(ns bliss.osc-events
  (:use overtone.osc
        bliss.sequencer))

(def get-cliento
  (memoize (fn [host port]
             (osc-client host port))))

(defn osc-action
  [self time]
  (assert (integer? (:port self)) "osc-bundle :port must be integer")
  (assert (string? (:path self)) "osc-bundle :path must be a string")
  (let [client (get-cliento
                  (:host self "localhost")
                  (:port self))]
    (in-osc-bundle client
                   time
                   (apply osc-send
                          client
                          (:path self)
                          (:values self)))))

(defn osc-event [& rest]
  (list
   (assoc (apply hash-map rest)
     :action osc-action)))

(def ^:dynamic *testevent*
  (osc-event :port 7770
             :path "/out"
             :values [8]))

(osc-send (osc-client "localhost" 7770) "/out" 0)
