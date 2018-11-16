(ns halftone.app
  (:require [quil.core :as q])
  (:import (processing.svg PGraphicsSVG))
  (:gen-class))

(set! *warn-on-reflection* true)

(defn normalize-angle [angle]
  (let [adj (+ 90 (* -1 angle))]
    (if (<= adj -90)
      (+ 180 adj)
      adj)))

(defn normalize-angle2 [angle]
  (+ 90 (* -1 angle)))

(defn to-radians ^double [^double degrees]
  (java.lang.Math/toRadians degrees))

(defn compute-rise-and-run [angle-b side-a]
  (let [angle-a 90
        angle-c (- 180 (+ angle-a angle-b))
        a-over-sin-side-a (/ side-a (java.lang.Math/sin ^double (to-radians angle-a)))
        side-b (* (java.lang.Math/sin ^double (to-radians angle-b))
                  a-over-sin-side-a)
        side-c (* (java.lang.Math/sin ^double (to-radians angle-c))
                  a-over-sin-side-a)]
    {:run side-b
     :rise side-c}))

(defn dotvector-seq [x y angle dot-distance]
  (let [normalized-angle (normalize-angle angle)
        {:keys [run rise]} (compute-rise-and-run normalized-angle dot-distance)]
    (iterate (fn [{:keys [x y]}]
               {:x (+ x run)
                :y (+ y rise)})
             {:x x
              :y y})))

(defn dotvector-seq2 [x y angle dot-distance]
  (let [normalized-angle (normalize-angle2 angle)
        {:keys [run rise]} (compute-rise-and-run normalized-angle dot-distance)]
    (iterate (fn [{:keys [x y]}]
               {:x (+ x run)
                :y (+ y rise)})
             {:x x
              :y y})))

(def dot-distance 120)
(def dot-size 70)

(defn draw-dots [rgba angle dot-size dot-distance width height & opts]
  (let [start-pos -33
        angle-2 (- angle 90)
        angle-3 (+ angle 90)
        boundary-predicate (fn [{:keys [x y]}]
                             (and (not (> x (+ width dot-distance)))
                                  (not (> y (+ height dot-distance)))))]

    (q/with-fill rgba
      (doseq [starter (take 100 (dotvector-seq2 (if (> angle-3 180)
                                                  (+ width start-pos)
                                                  start-pos)
                                                start-pos
                                                angle-2
                                                dot-distance))
              {:keys [x y]} (take-while
                             boundary-predicate
                             (dotvector-seq (:x starter)
                                            (:y starter)
                                            angle
                                            dot-distance))]
        (when (and (< x (+ width dot-size))
                   (> x start-pos)
                   (< y (+ height dot-size))
                   (> y start-pos))
          (q/ellipse x
                     y
                     dot-size
                     dot-size)))

      (doseq [starter (->> (dotvector-seq2 (if (> angle-3 180)
                                             (+ width start-pos)
                                             start-pos)
                                           start-pos
                                           angle-3
                                           dot-distance)
                           (drop 1)
                           (take 100))
              {:keys [x y]} (take-while
                             boundary-predicate
                             (dotvector-seq (:x starter)
                                            (:y starter)
                                            angle
                                            dot-distance))]
        (when (and (< x (+ width dot-size))
                   (> x start-pos)
                   (< y (+ height dot-size))
                   (> y start-pos))
          (q/ellipse x
                     y
                     dot-size
                     dot-size))))))

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn draw []
  (let [cyan (future
               (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "cyan3.svg")
                 (q/no-stroke)
                 (draw-dots [10 120 200 190] 60 dot-size dot-distance 800 500)))

        magenta (future (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "magenta3.svg")
                          (q/no-stroke)
                          (draw-dots [200 10 100 190] 45 dot-size dot-distance 800 500)))

        yellow (future (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "yellow3.svg")
                         (q/no-stroke)
                         (draw-dots [250 200 10 190] 75 dot-size dot-distance 800 500)))

        black (future (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "black3.svg")
                        (q/no-stroke)
                        (draw-dots [0 0 0 45] 110 dot-size dot-distance 800 500)))

        all (future (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "all3.svg")
                      (q/no-stroke)
                      ;; cyan
                      (draw-dots [10 120 200 190] 60 dot-size dot-distance 800 500)
                      ;; magenta
                      (draw-dots [200 10 100 190] 45 dot-size dot-distance 800 500)
                      ;; yellow
                      (draw-dots [250 200 10 190] 75 dot-size dot-distance 800 500)
                      ;; black
                      (draw-dots [0 0 0 45] 110 dot-size dot-distance 800 500)))]
    (time
     @(future
        @cyan
        @magenta
        @yellow
        @black
        @all))))

(try (q/defsketch example
       :title "halftone"
       :settings #(q/smooth 8)
       :setup setup
       :draw draw
       :size [800 500]
       :renderer :svg
       :output-file "out5.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
