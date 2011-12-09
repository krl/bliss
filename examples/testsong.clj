(ns bliss-test
  (:use overtone.live
        bliss.sequencer
        bliss.play
        bliss.sc-events))

(do
  (def bd (sample-bundle "examples/samples/bd.aif"))
  (def sn (sample-bundle "examples/samples/sn.aif"))
  (def ch (sample-bundle "examples/samples/ch.aif"))
  (def oh (sample-bundle "examples/samples/oh.aif")))

(def-inst-bundle form [freq   100
                       bw     100
                       len    1
                       amp    0.5]
  (let [env  (env-gen (lin-env 0.02 len 0.02) :action FREE)
        sig  (formant freq (+ freq (* (sin-osc:kr (* len 6) 0) freq)) 1)]        
    (* amp env sig)))

(form)

(def-inst-bundle sub [freq 100
                       len  1
                       amp  0.5]
  (let [env    (env-gen (lin-env 0.1 len 0.1) :action FREE)
        sig    (bpf (pink-noise) freq 0.01)
        filter (rlpf sig freq 0.1)]
    (* amp env (+ (* filter 10) (/ 10 sig)))))

(defn msub [freq]
  (jn-n 2
        (jn-nv j 7
               (sub :freq (+ (* j freq) (rand))))))

(defn song []
  (let [len (/ 1 (oneafter 2 2 3))
        freq (if (end-of 16)
               (* 40 3/2)
               (* 80 (oneafter 1/2 1/2 4/5 3/5)))]
    (jn
     (ass :amp 0.1
          (msub freq))

     (form :freq freq
           :len  len)
     
     (cc-n 8 (jn-nv voice 8 (ch :len 1/8
                                :amp  (+ (/ 0.02 (/ voice 2)) (rand 0.02))
                                :rate (+ (* 0.2 voice) (rand 0.005)))))

     (off 1/4 (cc-n 2 (oh :amp 0.02 :len 1/8)))
     
     (oneafter
      (jn-n 8
             (cc-no c (oneafter [3/4 1/4]
                                [3/4 1/4]
                                [2/4 2/4])
                    (bd :rate (+ 1/2 (rand 0.3)) :len c :amp 0.3)))
      
      (jn-nv v 8 (sn :rate (* 2/8 v)
                     :len 1/16
                     :amp 0.4))))))

(stop-all-processes)

(start-process song)
