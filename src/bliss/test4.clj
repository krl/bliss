(ns bliss-test
  (:use overtone.live
        bliss.sequencer
        ;; bliss.scales
        bliss.play
        bliss.helpers
        bliss.sc-events))

;; (do
;;   (def bd (sample-bundle 0   "/home/krille/samples/a43/9700_A43_A43_thudkick.aif"))
;;   (def sn (sample-bundle 1   "/home/krille/samples/a43/11378_A43_A43_808_snare.aif"))
;;   (def ch (sample-bundle 2   "/home/krille/samples/a43/9696_A43_A43_606_cHH.aif"))
;;   (def oh (sample-bundle 2.1 "/home/krille/samples/a43/9878_A43_A43_808_oHH.aif")))

(def-inst-bundle form [freq   100
                       bw     100
                       len    1
                       amp    0.5
                       res    0.5]
  (let [env  (env-gen (lin-env 0.02 len 0.02) :action FREE)
        sig  (formant freq          (+ freq (* (sin-osc:kr (/ 1 len) 0.1) 20)) bw)
        cut  (rlpf (saw freq) (* freq 2) 0.0001)]
    (* amp env (+ sig
                  (* cut 0.01)))))

(def-inst-bundle sub [freq 200
                      len  1
                      amp  1]
  (let [env   (env-gen (lin-env 0.02 len 0.02) :action FREE)
        noise (brown-noise)
        band  (bpf noise freq 0.01)]
    (* amp band)))
        
(defn song [state]
  (sub))

(defn song [state]
  (ass :freq (* 34 (oneof 1 4/5 3/4))
       :len  (oneof 2 3)
       (jn-n 6
             #(form :amp  0.7
                    :bw   10))))

(stop-all-processes)

(start-process (count-measures song))

(defn plonk [state]
  (let [len (rand)]
    (sinus)))