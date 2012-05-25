(ns bliss-test
  (:use overtone.live
        [clojure.contrib.math :only [expt]]
        bliss.sequencer
        bliss.scales
        bliss.play
        bliss.helpers        
        [bliss.draw :only [display-process]]
        bliss.sc-events)
  (:use clojure.contrib.trace))

(do
  (def bd (sample-bundle 0   "/home/krille/samples/a43/9700_A43_A43_thudkick.aif"))
  (def sn (sample-bundle 1   "/home/krille/samples/a43/11378_A43_A43_808_snare.aif"))
  (def ch (sample-bundle 2   "/home/krille/samples/a43/9696_A43_A43_606_cHH.aif"))
  (def oh (sample-bundle 2.1 "/home/krille/samples/a43/9878_A43_A43_808_oHH.aif")))

(def-inst-bundle sinus [freq   21
                        len    1
                        cut    200
                        amp    0.5
                        smooth 0.5
                        res    0.5]
  (let [env  (env-gen (lin-env smooth len smooth) :action FREE)
        sig  (saw freq)
        nois (+ 
              (* sig 0.6)
              (* (brown-noise) 0.6))
        cut  (hpf (rlpf nois cut res) 50)]
    (* cut env amp)))

(defn song [state]
  (let [len (oneof 3 4)]
    (jn (cc-l len (fn [beat]
                    (jn-n 4 #(bd :amp  0.3
                                 :len  2
                                 :rate (* % 0.4)))))

        (off (- len 1/3) (cc-l 1/3
                               #(sn :amp 0.1 :len 1/6)))
        
        (jn-n 10 (fn [voice]
                   (let [hatlen  (+ (/ (oneof 1 2) (oneof 2 3)) (rand 0.002))
                         hatrate (+ 0.3 (rand 0.8))]
                     (cc-l len #((oneof sn ch bd bd bd)
                                 :amp  (rand 0.04)
                                 :len  hatlen
                                 :rate hatrate)))))
        (jn-n 8
              #(sinus :freq (* 1/2
                               (oneof 3 4)
                               (+ 1 (rand 0.001)))
                      :cut (+ 10 (* 20 (rand-int 30)))
                      :res 0.04
                      :amp 0.08
                      :smooth (rand 1)
                      :len len)))))

(stop-all-processes)

(start-process (count-measures song))

(defn plonk [state]
  (let [len (rand)]
    (sinus)))