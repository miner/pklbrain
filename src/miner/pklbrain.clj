(ns miner.pklbrain
  (:require [clojure.pprint :refer [pprint]]
            [quil.core :as q]
            [quil.middleware :as qm]))

(defn foo []
  :pickleball)

(defn update-state [state]
  (let [xf (quot (q/width) 30)
        yf (quot (q/height) 54)]
    (assoc state :scale (min xf yf))))

(defn draw-scaled [shape-fn scale & args]
  (apply shape-fn (mapv #(* % scale) args)))



(defn draw-ball [state]
  (let [[x y] (get-in state [:ball :location])]
    (q/stroke-weight 1)
    (q/fill 255 204 0)
    (draw-scaled q/ellipse (:scale state) x y 1 1)))

(defn draw-player [state player]
  (let [scale (:scale state)
        p (get state player)
        [tx ty] (:location p)
        hand (:hand p :right)]
    (q/fill 10 20 30)
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
          



(defn draw-state [state]
  (q/push-style)
  (q/background 200)
  (q/fill 128)
  (q/stroke 120)
  ;; center co-ords, +x to left, +y down
  (let [scale (:scale state)
        hw (quot (q/width) 2)
        hh (quot (q/height) 2)
        [tx ty] (:triangle state)
        sc (fn [x] (* x scale))]
    (q/with-translation [hw hh]
      (q/fill 180)
      (q/stroke-weight 4)
      (draw-scaled q/rect scale -10 -22 20 44)
      (q/fill 150)
      (draw-scaled q/rect scale -10 -7 20 14)
      (draw-scaled q/line scale -10 0 10 0)
      (draw-scaled q/line scale 0 7 0 22)
      (draw-scaled q/line scale 0 -7 0 -22)

      (draw-ball state)

      (draw-player state :a)
      (draw-player state :b)
      (draw-player state :c)
      (draw-player state :d)
      )))
  

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

(defn setup []
  (let [xf (quot (q/width) 30)
        yf (quot (q/height) 54)]
    {:scale (min xf yf)
     :ball {:location [5 20]}
     :a {:location [5 22] :label "A"}
     :b {:location [-5 22] :label "B" :hand :left}
     :c {:location [5 -22] :label "C"}
     :d {:location [-5 -22] :label "D"}}))





(defn my-test []
  (let [sk (q/sketch 
               :host "host"
               :title "PKL Brain"
               :size [600  1000]
               :setup setup
               :update update-state
               :draw draw-state
               :mouse-clicked clicked
               :key-pressed key-pressed
               :features [:resizable]
               :middleware [qm/fun-mode])]
    sk))

