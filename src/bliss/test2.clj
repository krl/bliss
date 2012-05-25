(ns bliss-test
  (:use overtone.live
        ;; [clojure.contrib.math :only [expt]]
        bliss.sequencer
        ;; bliss.scales
        bliss.play
        bliss.helpers        
        ;; [bliss.draw :only [display-process]]
        bliss.sc-events))
  ;; (:use clojure.contrib.trace))

(do
  (def bd (sample-bundle 0   "/home/krille/samples/a43/9700_A43_A43_thudkick.aif"))
  (def sn (sample-bundle 1   "/home/krille/samples/a43/11378_A43_A43_808_snare.aif"))
  (def ch (sample-bundle 2   "/home/krille/samples/a43/9696_A43_A43_606_cHH.aif"))
  (def oh (sample-bundle 2.1 "/home/krille/samples/a43/9878_A43_A43_808_oHH.aif")))

(defn phat [samp & args]
  (apply ass1
         (jn-n 16 #(samp :rate (+ 0.5 (rand 0.5))))
         args))

(defn beginning-of [state length]
  (zero? (mod (read-var state :measure 0) length)))

(defn end-of [state length]
  (= (mod (read-var state :measure 0) length)
     (- length 1)))
  
(defn song [state]
  (jn
   (off 1/3
        (cc-n (+ (mod (read-var state :measure 0) 4) 2)
              (fn [seq]
                (jn-n 10
                      #(bas :freq (+ 180 (rand 0.01))
                            :cut (* % (/ seq 6) 100)
                            :res 0.001
                            :len 1/3
                            :amp 0.9)))))
   (cc
    (let [num (if (beginning-of state 4) 2 1)]
      (cc-n num #(phat bd :len (/ 1 num))))
    (if (end-of state 8)
      (cc-n 3 #(phat sn :len 1/3))
      (phat sn)))
   (cc
    (cc-n 4 #(phat ch :len 1/4 :amp (+ 0.1 (rand 0.2))))
    (if (not (end-of state 8))
      (cc-n 4 #(phat ch :len 1/4 :amp (+ 0.1 (rand 0.2))))
      (phat oh)))))

(stop-all-processes)

(start-process (count-measures song))

(def-inst-bundle bas [freq 21
                      cut  80
                      res  1
                      trem 180
                      len  1
                      amp  0.5]
  (let [env  (env-gen (lin-env 0.001 (* len 0.4) 0.001) :action FREE)
        sig  [(saw freq) (saw (* freq 1.01))]
        cut  (rlpf sig cut res)
        trem (* cut (+ 0.5 (sin-osc trem)))]
    (* trem amp env)))

(stop-all-processes)