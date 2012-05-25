(ns bliss.scales
  (:use bliss.sequencer
        ;; [clojure.math :only [expt]]
        ))

(def MAJOR '(2 2 1 2 2 2 1))
(def MINOR '(2 1 2 2 1 2 2))

(def PRIME  0)
(def SECOND 1)
(def THIRD  2)
(def FOURTH 3)
(def FIFTH  4)

(defn rotate [list]
  (let [list (reverse list)]
    (reverse (conj (butlast list) (last list)))))

(defn interval [tones scale]
  (loop [tones     tones
         semitones 0
         scale     scale]
    (println scale)
    (if (zero? tones)
      semitones
      (recur (dec tones)
             (+ semitones (first scale))
             (rotate scale)))))

(defn scale-frequency [freq semitones]
  (* freq (expt 2 (/ semitones 12))))
  
(defn resolve-freq [context bundle]
  (assert (bundle? bundle))
  (ass :freq #(+ (scale-frequency (:base context)
                                  (interval (+ (or (:note %) 0)
                                               (or (:offset context) 0)
                                               (* (or (:octave %) 1) 12))
                                            (or (:scale context) MAJOR))))
       bundle))