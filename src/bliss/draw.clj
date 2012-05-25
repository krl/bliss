;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns bliss.draw
  (:use [penumbra opengl]
        bliss.helpers
        [clojure.contrib.generic.math-functions :only [log]])
  (:require [penumbra.app :as app]
            [penumbra.text :as text]))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init [state]
  (app/title! "Gears")
  (app/vsync! false)
  (enable :depth-test)
  (enable :line-smooth)
  ;; (enable :multisampling)
  (enable :lighting)
  (enable :light0)
  (shade-model :flat))

(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  (translate 0 0 -10)
  (light 0 :position [1 1 1 0])
  state)

(defn close [state]
  (swap! (:playing* @(:process* state)) (fn [_] false)))

(defn mouse-drag [[dx dy] _ button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn key-press [key state]
  (cond
   (= :escape key) (app/pause!)))

(defn draw-process [process]
  (doseq [event (:history process)]
    (push-matrix
     (translate (get-time event)
                (or (:channel event)
                    (log (:freq event)))
                0)
     (draw-polygon
      (vertex 0                    0                    0)
      (vertex 0                    1                    0)
      (vertex (:len event)         0.5                  0)))))


(defn display [[delta time] state]
  ;; (text/write-to-screen (format "%d fps" (int (/ 1 delta))) 0 0)
  (let [process @(:process* state)]
  
    (rotate (:rot-x state) 1 0 0)
    (rotate (:rot-y state) 0 1 0)
    ;; (rotate (* 20. (rem time 360)) 0.5 1 1)

    (push-matrix
     (scale 0.5 0.5 0.5)

     ;; draw 0-timeline
     (line-width 1.8)
     (draw-lines
      (vertex 0 -20 0)
      (vertex 0  20 0))

     ;; draw events
     (push-matrix ; time relative matrix
      (translate (/ (- (:start-time process) (timestamp)) 1000.0) -11 0)
      (draw-process process)

      ;; draw last write
      (push-matrix
       (translate (:last-write process) -11 0)

       (draw-lines
        (vertex 0 -20 0)
        (vertex 0  20 0)))))

    (app/repaint!)))

;;By using this instead of display, we can recompile display and see
;;our changes immediately.  Try reversing the sign on rotate while the
;;app is running and see for yourself.
(defn display-proxy [& args]
  (apply display args))

(defn display-process [process*]
  (future
    (app/start
     {:reshape reshape
      :display display-proxy
      :init init
      :close close
      :mouse-drag mouse-drag
      :key-press key-press}
     ;; state
     {:rot-x 0
      :rot-y 0
      :process* process*})))