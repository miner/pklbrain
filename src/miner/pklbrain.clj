(ns miner.pklbrain
  (:require [clojure.pprint :refer [pprint]]
            [quil.core :as q]
            [quil.middleware :as qm]))

(defn foo []
  :pickleball)

(defn rgba [n]
  "Alpha is the high byte on the integer n, then R, G, B bytes.  Returns vector of [R G B A]"
  (vector (bit-and (bit-shift-right n 16) 0xFF)
          (bit-and (bit-shift-right n 8) 0xFF)
          (bit-and n 0xFF)
          (bit-and (bit-shift-right n 24) 0xFF)))

(defn color-int
  ([r g b] (color-int r g b 0))
  ([r g b a] {:pre [(<= 0 r 255) (<= 0 g 255) (<= 0 b 255) (<= 0 a 255)]}
   (bit-or (bit-shift-left a 24)
            b
            (bit-shift-left g 8)
            (bit-shift-left r 16))))


(defn update-state [state]
  (let [w (q/width)
        h (q/height)]
    (if (< w h)
      (let [xf (quot w 24)
            yf (quot h 50)]
        (assoc state :scale (min xf yf) :rotation 0))
      (let [xf (quot h 24)
            yf (quot w 50)]
        (assoc state :scale (min xf yf) :rotation q/HALF-PI)))))


;; FIXME: probably should just use q/scale
;; But, q/scale blows up lines.  Look at stroke size to figure out.
;; Need to consider odd number of args that might not be coordinates.

(defn draw-scaled
  ([shape-fn scale]   (shape-fn))
  ([shape-fn scale a]   (shape-fn (* scale a)))
  ([shape-fn scale a b]   (shape-fn (* scale a) (* scale b)))
  ([shape-fn scale a b c]   (shape-fn (* scale a) (* scale b) (* scale c)))
  ([shape-fn scale a b c d]   (shape-fn (* scale a) (* scale b) (* scale c) (* scale d)))
  ([shape-fn scale a b c d e]   (shape-fn (* scale a) (* scale b) (* scale c) (* scale d)
                                          (* scale e)))
  ([shape-fn scale a b c d e & more]
   (apply shape-fn (mapv #(* % scale) (list* a b c d e more)))))

;; SEM experiment with ignoring scale here and just -- looks bad with stroke getting scaled
;; too!  Probably better to keep my manual resizing

#_
(defn draw-scaled [shape-fn scale & args]
  (apply shape-fn args))

;; Ball is bigger than scale, but we want it big!
;; FIXME: when the ball bounces put an X through it or something.
(defn draw-ball [state]
  (let [[x y] (get-in state [:ball :location])]
    (q/stroke-weight 1)
    (q/fill 255 204 0)
    (draw-scaled q/ellipse (:scale state) x y 1 1)))

#_
(defn draw-player [state player]
  (let [scale (:scale state)
        p (get state player)
        [tx ty] (:location p)
        hand (:hand p :right)]
    (q/stroke 10)
    (q/fill 10 120 230)
    (if (neg? ty)
      (draw-scaled q/triangle scale tx ty (- tx -2) (+ ty -1) (+ tx -2) (+ ty -1))
      (draw-scaled q/triangle scale tx ty (- tx 2) (+ ty 1) (+ tx 2) (+ ty 1)))
    (if (neg? ty)
      (if (= hand :right)
        (draw-scaled q/ellipse scale (- tx 2) ty 2 1)
        (draw-scaled q/ellipse scale (+ tx 2) ty 2 1))
      (if (= hand :right)
        (draw-scaled q/ellipse scale (+ tx 2) ty 2 1)
        (draw-scaled q/ellipse scale (- tx 2) ty 2 1)))))
          

(defn draw-player [state player]
  (let [scale (:scale state)
        p (get state player)
        [tx ty] (:location p)
        hand (:hand p :right)]
    (q/stroke 10)
    (q/stroke-weight 1)

    (apply q/fill (or (:color p) [10 120 230]))
    ;;    (q/fill 10 120 230)

    (if (neg? ty)
      (draw-scaled q/triangle scale tx ty (- tx -1) (+ ty -1) (+ tx -1) (+ ty -1))
      (draw-scaled q/triangle scale tx ty (- tx 1) (+ ty 1) (+ tx 1) (+ ty 1)))
    (q/rect-mode :center)
    (if (neg? ty)
      (if (= hand :right)
        (draw-scaled q/rect scale (- tx 1) ty 1 0.5 1)
        (draw-scaled q/rect scale (+ tx 1) ty 1 0.5 1))
      (if (= hand :right)
        (draw-scaled q/rect scale (+ tx 1) ty 1 0.5 1)
        (draw-scaled q/rect scale (- tx 1) ty 1 0.5 1)))
    (q/rect-mode :corner)))



(defn draw-state [state]
  (q/push-style)
  (q/background 200)
  (q/fill 128)
  (q/stroke 120)
  ;; center co-ords, +x to left, +y down
  (let [scale (:scale state)
        rotation (:rotation state)
        hw (quot (q/width) 2)
        hh (quot (q/height) 2)
        [tx ty] (:triangle state)
        sc (fn [x] (* x scale))]
    (q/with-translation [hw hh]
      (q/with-rotation [rotation]

        (q/fill 230 230 250)
        (q/stroke 255)
        (q/stroke-weight 4)

        ;;(q/scale scale)

        (draw-scaled q/rect scale -10 -22 20 44)

        (q/fill 200 200 250)
        (draw-scaled q/rect scale -10 -7 20 14)
        (draw-scaled q/line scale 0 7 0 22)
        (draw-scaled q/line scale 0 -7 0 -22)




        ;; net
        (q/stroke 128)
        (draw-scaled q/line scale -11 0 11 0)

        (q/stroke 0)

        (draw-player state :a)
        (draw-player state :b)
        (draw-player state :c)
        (draw-player state :d)

        (draw-ball state)
        )))
  (q/pop-style))
  

(defn clicked [state mouse-info]
  (println "\nclicked at:" (q/millis))
  (println "mouse at:" mouse-info)
  (pprint state)
  state)

(defn key-pressed [state key-info]
  ;;(println "\nkey:" (q/key-as-keyword) key-info)
  (case (long (:key-code key-info))
    ;;    (32) (update state :running? not)
    ;;    (9, 10, 13) (update state :orbits? not)
    state))

;; color is a vector of [r g b alpha] -- alpha is optional
;; coords are x, y in feet.  0,0 is center net.  Positive x to right.  Positive y down.
;;   One baseline is -10, -22 to +10, -22 (top of screen)
;;   Other baseline is -10, +22 to +10, +22
;;   "A" is first server, from ++ court, bottom right.
;;
;; If aspect ration changes to landscape, then assignments rotate 90 deg.  Positve X goes
;; down, positive Y goes left.

(defn setup []
  (update-state {:ball {:location [5 22]}
                 :a {:location [5 22] :label "A" :color [164 164 64]}
                 :b {:location [-5 22] :label "B" :hand :left :color [0 255 200]}
                 :d {:location [5 -7] :label "D" :color [10 128 255]}
                 :c {:location [-5 -22] :label "C" :hand :left :color [220 38 205]}}))



(defn my-test []
  (let [sk (q/sketch 
               :host "host"
               :title "PKL Brain"
               :size [600  1000]
               :setup setup
               :update #'update-state
               :draw #'draw-state
               :mouse-clicked #'clicked
               :key-pressed #'key-pressed
               :features [:resizable]
               :middleware [qm/fun-mode])]
    sk))


(defn hack [sk key-path val]
  (let [state-atom (:state (.state ^quil.Applet sk))]
    (swap! state-atom update-in key-path (fn [_old new] new) val)))


(comment
  (require '[quil.core :as q])
  (my-test)

  )
