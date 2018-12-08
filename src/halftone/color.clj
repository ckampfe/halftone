(ns halftone.color
  (:require [quil.core :as q]
            [clojure.core.matrix :as m]))

(m/set-current-implementation :vectorz)

(defn clamp [minimum maximum]
  (fn [n]
    (if (< n minimum)
      minimum
      (if (> n maximum)
        maximum
        n))))

(def clamp-to-one (clamp 0 1))

(defn rgb-cosine-similarity [v1 v2]
  (let [a1 (m/array v1)
        a2 (m/array v2)
        m1 (m/magnitude a1)
        m2 (m/magnitude a2)
        dot (m/dot a1 a2)]
    (/ dot (* m1 m2))))

(defn temperature
  "temperature falls as k-over-kmax increases"
  [k-over-kmax]
  (* 1 k-over-kmax))

(defn annealing-probability
  "temp is rational value between 1 and 0.

   When T tends to zero, the probability P(e,e',T) must tend to zero if e' > e and to a positive value otherwise.

  energy-s and energy-new-s are both floats, the consine sim of target and state (or new prospective state)"
  [energy-s energy-new-s temperature]
  (if (> energy-new-s energy-s)
    1.0
    (- 1 (Math/pow (- energy-s energy-new-s) temperature))))

(defn random-new-neighbor [[r g b] temperature]
  [(rand 256)
   (rand 256)
   (rand 256)])

(defn simulated-annealing
  "When T tends to zero, the probability P(e,e',T) must tend to zero if e' > e and to a positive value otherwise."
  [target initial-s kmax]
  (let [energy rgb-cosine-similarity]
    (reduce (fn [s k]
              (let [temperature (temperature (- 1 (/ k kmax)))
                    s-new (random-new-neighbor s temperature)]
                (if (>= (annealing-probability (energy target s)
                                               (energy target s-new)
                                               temperature)
                        (rand))
                  s-new
                  s)))
            initial-s
            (range kmax))))

(defn normalized-rgb->cmyk
  "normalized (to 1) rgb to cmyk
  https://www.rapidtables.com/convert/color/rgb-to-cmyk.html

  Black   = 1-max(1-Red,1-Green,1-Blue)
  Cyan    = (1-Red-Black)/(1-Black)
  Magenta = (1-Green-Black)/(1-Black)
  Yellow  = (1-Blue-Black)/(1-Black)
  "
  [[^double red ^double green ^double blue]]
  (let [black (- 1 (max (- 1 red)
                        (- 1 green)
                        (- 1 blue)))

        minus-one-black (- 1 black)

        cyan (clamp-to-one (/ (- 1 (- red black))
                              minus-one-black))

        magenta (clamp-to-one (/ (- 1 (- green black))
                                 minus-one-black))

        yellow (clamp-to-one (/ (- 1 (- blue black))
                                minus-one-black))]
    {:cyan cyan
     :magenta magenta
     :yellow yellow
     :k black}))

(defn normalize-color-value-to-1 [n]
  (/ n 255))

(defn rgb->cmyk
  ([rgb] (normalized-rgb->cmyk (map normalize-color-value-to-1 rgb)))
  ([r g b] (rgb->cmyk [r g b])))

(def xdim 700)
(def ydim 700)

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn draw []
  (q/no-stroke)
  (let [targets [[200 50 30] ;; red
                 [30 200 100] ;; green
                 [0 100 255]]] ;; blue

    (doseq [i (range (count targets))]
      (let [result (simulated-annealing (nth targets i) [120 120 120] 1000)]

        (q/with-fill (nth targets i)
          (q/rect 50
                  (* (+ i 1) 50)
                  40
                  40))

        (q/with-fill result
          (q/rect 100
                  (* (+ 1 i) 50)
                  40
                  40))))))

(q/defsketch example
  :title "m"
  :settings #(q/smooth 1)
  :setup setup
  :draw draw
  :size [xdim ydim]
  ;; :renderer :svg
  ;; :output-file "out5.svg"
  :features [:no-bind-output])

(comment
  (def yellowish-color [169 179 42])

  (def burnt-orange [178 107 62])

  (rgb->cmyk burnt-orange)

;; greenish
  (rgb->cmyk [13 107 62])

  (normalized-rgb->cmyk (map normalize-color-value-to-1 yellowish-color)))
