(ns lens.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (q/ellipse-mode :radius)
  {:num-rays 9
   :lens-radius 200
   :refraction-index 1.5
   :x (/ (q/width) 3)
   :y (/ (q/height) 2)
   :lens-x (* (q/width) 2/3)
   :lens-y (/ (q/height) 2)})

(defn update-state [state]
  (let [pressed (q/key-as-keyword)
        dx (case pressed
             :h -1
             :H -10
             :l 1
             :L 10
             0)
        dy (case pressed
             :j 1
             :J 10
             :k -1
             :K -10
             0)]
    (assoc state
           :x (+ (:x state) dx)
           :y (+ (:y state) dy)))
  )

(defn draw-lines [lines]
  (if (not= 0 (count lines))
    (do
      (apply q/line (first lines))
      (recur (rest lines)))))

(defn refract-ray [ray normal refraction]
  "Refracts a ray with a given normal vector and ratio of indices of refraction."
  (let [magnitude (fn [v] (Math/sqrt (apply + (map * v v))))
        antinormal (map #(* -1.0 %) normal)
        cross-prod (/ (- (* (first ray) (last antinormal)) (* (first antinormal) (last ray)))
                      (magnitude ray)
                      (magnitude normal))
        angle-in (Math/asin cross-prod)
        angle-out (if (> 0 (apply + (map * ray normal)))
                    (/ angle-in refraction)
                    (* angle-in refraction))
        dangle (- angle-in angle-out)
        s (Math/sin dangle)
        c (Math/cos dangle)
        rotated [(- (* c (first ray)) (* s (last ray)))
                 (+ (* s (first ray)) (* c (last ray)))]]
    rotated))

(defn cast-ray [source ray boundary]
  "Finds the intersection that a ray has with a boundary."
  ; ray is assumed to be a unit vector
  ;; [[a b]  [[t]    [[e]
  ;;  [c d]]  [s]] =  [f]]
  (let [boundary-ray (map - (last boundary) (first boundary))
        a (first ray)
        b (first boundary-ray)
        c (last ray)
        d (last boundary-ray)
        bound-source (map - (last boundary) source)
        e (first bound-source)
        f (last bound-source)
        top (- (* a f) (* c e))
        bottom (- (* a d) (* b c))]
    (if (not= 0.0 bottom)
      (let [s (/ top bottom)]
        (if (and (> s 0) (< s 1))
          (map + (map #(* -1 s %) boundary-ray) (last boundary)))))))


(defn refract-line [line state]
  "refract a line where the second coordinate pair is at the surface of the lens."
  (let [vl (map - (last line) (first line))
        vr (map - [(:lens-x state) (:lens-y state)] (last line))
        magnitude (fn [v] (Math/sqrt (apply + (map * v v))))
        cross-prod (- (* (first vl) (last vr)) (* (first vr) (last vl)))
        angle-in (Math/asin (/ cross-prod (magnitude vl) (magnitude vr)))
        angle-out (Math/asin (/ (Math/sin angle-in) (:refraction-index state)))
        dangle (- angle-out angle-in)
        s (Math/sin dangle)
        c (Math/cos dangle)
        rotated [(- (* c (first vr)) (* s (last vr)))
                 (+ (* s (first vr)) (* c (last vr)))]]
    [(last line) (map + (last line) (map #(* 4 %) rotated))]))

(defn draw-rays [state]
  "Draw the rays of light coming from source and going through lens."
  (let [dx (- (:lens-x state) (:x state))
        dy (- (:lens-y state) (:y state))
        distance (Math/sqrt (+ (* dx dx) (* dy dy)))
        angular-radius (Math/asin (/ (:lens-radius state) distance))
        dangle (/ angular-radius (:num-rays state))
        rays (for [i (range (inc (* -1 (:num-rays state))) (:num-rays state))]
               (let [angle (* i dangle)
                     c (Math/cos angle)
                     s (Math/sin angle)]
                 [(- (* c dx) (* s dy))
                  (+ (* s dx) (* c dy))]))
        intersection (fn [x y vx vy bx by r]
                       (let [a (+ (* vx vx) (* vy vy))
                             b (* -2.0 (+ (* vx (- x bx)) (* vy (- y by))))
                             c (+ (Math/pow (- x bx) 2) (Math/pow (- y by) 2) (* -1.0 (* r r)))
                             t (/ (- (Math/sqrt (- (* b b) (* 4.0 a c))) b) 2.0 a)]
                         [(- x (* t vx)) (- y (* t vy))]))
        vacuum-lines (for [ray rays] [[(:x state) (:y state)]
                                      (intersection (:x state) (:y state)
                                                    (first ray) (last ray)
                                                    (:lens-x state) (:lens-y state)
                                                    (:lens-radius state))])
        lens-lines (for [line vacuum-lines] (refract-line line state))
        ]
    (q/no-fill)
    (q/stroke-weight 2)
    (q/stroke 255)
    (draw-lines vacuum-lines)
    (q/stroke 255 128 128)
    (draw-lines lens-lines)
  ))

(defn draw-state [state]
  (q/background 0)
  (q/fill 0 127 255)
  (q/no-stroke)
  (q/ellipse (:lens-x state) (:lens-y state) (:lens-radius state) (:lens-radius state))
  (draw-rays state)
  )


(q/defsketch lens
  :title "Lens demo"
  :size [768 432]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
