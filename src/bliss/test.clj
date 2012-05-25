(ns bliss-test
  (:use overtone.live
        bliss.sequencer
        bliss.scales
        bliss.play
        bliss.helpers        
        bliss.sc-events)
  (:use clojure.contrib.trace))

(do
  (def bd (sample-bundle 0   "/home/krille/samples/a43/9700_A43_A43_thudkick.aif"))
  (def sn (sample-bundle 1   "/home/krille/samples/a43/11378_A43_A43_808_snare.aif"))
  (def ch (sample-bundle 2   "/home/krille/samples/a43/9696_A43_A43_606_cHH.aif"))
  (def oh (sample-bundle 2.1 "/home/krille/samples/a43/9878_A43_A43_808_oHH.aif")))

(def-inst-bundle bas [freq 21
                      cut  80
                      res  1
                      len  1
                      amp  0.5]
  (let [env (env-gen (perc 0.01 len) :action FREE)
        sig [(saw freq) (saw (* freq 1.001))]
        cut (rlpf sig cut res)]
    (* amp env cut)))


(stop)

(def-inst-bundle effem [freq  100
                        multi 2
                        len   1
                        amp   0.3]
  (let [env (env-gen (perc 0.01 len) :action FREE)]            
    (* amp env (sin-osc (+ freq (* (sin-osc (* multi freq)) 1000))))))

(def-inst-bundle spark [cut 100
                        spread 20
                        amp 0.5
                        len 1
                        res 0.4]
  (let [env (env-gen (perc 0.04 len) :action FREE)]
    (*
     (hpf 
      (rlpf [(white-noise) (white-noise)]
            (- cut spread)
            res)
      (+ cut spread))
     env
     amp)))

(defn testbas [state]
  (let [len (/ (rand-int 8) 8)]
    (cc 
     (cc-l len
           #((oneof bd sn oh ch)
             :len (rand 0.005)
             :amp 0.11
             :rate (- 2 (rand 1.6))))
     (jn-n 20        
           #(bas :freq (* 180 (rand-int-range 1 10))
                 :amp 0.3
                 :cut 50
                 :res 0.01
                 :len len)))))

(start-process testbas)
(inst-fx bas-proxy fx-distortion)
(inst-fx bas-proxy fx-echo)

(stop-all-processes)

(defn simpledrum [state fill]
  (jn
   (cc-n 8 #(ch :len 1/8 :amp (rand 1)))
   (cond
    fill
    ;; fill!   
    (scale-bundle 1/2
                  (cc
                   (bd)
                   (off 1/4 (sn :len 3/4))))
    :else             
    (scale-bundle 1/2
                  (cc
                   (bd)
                   (sn))))))

(defn bassline [state fill]
  (cc-l0 1
         #(bas :len (if fill 1/16 1/4)
               :octave 0
               :note (* % 2)
               :amp 1)))

(defn chords [state fill]
  (trim 1
        (off (if fill 1/8 0)
             (jn-n0 3 #(bas :octave 2
                            :note (* % 2)
                            :amp 0.4
                            :cut 200)))))

(defn song [state]
  (let [fill    (= (mod (read-var state :measure 0) 4) 3)
        context {:base 30
                 :offset (if fill FIFTH PRIME)
                 :scale MAJOR}]
    (resolve-freq
     context
     (jn
      (chords     state fill)
      (bassline   state fill)
      (simpledrum state fill)))))

(stop-all-processes)
(start-process-sync (count-measures song))

(defn sparkle [state]  
  (jn
   (cc-l 1 #(bd :len (oneof 1 4/5)))
   
   (cc-l 1 #(ch :len (oneof 1/5 1/10) :rate (/ 4 % (rand 1))))
   
   (off 2/5
        (cc-n 5 #(sn :len 1/10
                     :rate %
                     :amp (rand 1))))
   
   (ass :cut #(:freq %)
        :amp 0.6
        :res 0.01
        (cc
         (let [len (oneof 1 2)]
           (cc-n len #(bas :freq (* 120 (/ % 2))  :len (/ 2 len 5))))
         (bas :freq 60 :res 0.1 :len 3/5)))

   ;; (cc-l 1
   ;;       #(spark :len (oneof 1/5 1/10 3/10)
   ;;               :amp 0.2
   ;;               :cut 1000
   ;;               :spread 100
   ;;               :res 0.01))
   ))

(stop)

(start-process sparkle)

(stop-all-processes)

(defn pads [state]
  (scale-bundle
   0.6
   (let [len (oneof 4 4 3)]
     (resolve-freq
      {:base 40
       :scale MINOR
       :offset (oneof PRIME PRIME PRIME PRIME
                      THIRD THIRD
                      SECOND FOURTH FIFTH FIFTH)}
      (jn
       (jn-n 9 (fn [_]
                 (cc-l len #(oh :len (oneof 0.5 0.75 0.25)
                                :amp (rand 1)
                                :rate (+ 1 (rand 8))))))

       (cc-l len #(bd :len 1 :amp 1 :rate 1))
       
       (off 0.5 (cc-l (- len 0.5) #(sn :len 1
                                       :rate (+ 1 (rand 0.1)))))

       (cc-l len #(bas :octave 0 :note 0 :len (oneof 0.5 0.75) :amp 0.8 :res 0.3 :cut (+ 200 (* % 300))))
       
       (jn-n 4 (fn [voice]
                 (bas :octave 5 :note (* (- voice 1) 11) :multi 1 :len len :cut 1000)))
       )))))

(defn basse [state]
  (scale-bundle 0.4
                (jn
                 (resolve-freq
                  {:base 800
                   :scale MAJOR
                   :offset (oneof PRIME PRIME PRIME PRIME
                                  THIRD THIRD
                                  SECOND FOURTH FIFTH FIFTH)}
                  (jn (ass :amp 1
                           :cut 300
                           :res 0.4
                           (oneof (bas :note -48 :len 2)
                                  (cc 
                                   (bas :note -12 :len 0.5)
                                   (bas :note -36 :len 1.5))))
                      (cc-l 2
                            (fn [seq]
                              (jn-n 2
                                    #(bas :note (* (- seq 1) (* % 2) 1)
                                          :res (+ 0.1 (rand 0.9))
                                          :amp 0.2
                                          :cut (rand-int-range 100 8009)
                                          :len (oneof 1/2 1/4 1.5)))))))
                 (cc-n 8 #(ch :len 1/4 :amp (rand 1) :rate (+ 1 (rand 0.2))))
                 (cc (oneof (jn (bd) (bd :rate 0.8))
                            (cc (bd :len 1/4) (bd :len 3/4)))
                     (jn-n 8 #(sn :amp 1 :rate (+ 0.9 (rand 0.4))))))))

(defn test [state]
  (scale-bundle
   1
   (jn
    (resolve-freq
     {:scale MAJOR
      :offset (oneof PRIME PRIME PRIME FIFTH)
      :base  44}
     (jn-n 1 (fn [voice] 
               (cc-l 2 #(bas :octave 0
                             :note (+ (* (- % 1) (oneof 2 3)) (* voice 2))
                             :len 0.2
                             :amp 0.8
                             :cut 1000)))))
    (oh)
    (trim 2
          (cc-l 2 #(ch :len 1/3 :amp (+ 0.5 (rand 0.5))
                       :rate (+ 0.5 (rand 1))
                       ))))))

(stop-all-processes)
(display-process (start-process sparkle))

(start-process-sync sparkle)