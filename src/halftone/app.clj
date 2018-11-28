(ns halftone.app
  (:require [halftone.matrix :as hm]
            [clojure.core.matrix :as m]
            [quil.core :as q])
  (:import (processing.svg PGraphicsSVG))
  (:gen-class))

(set! *warn-on-reflection* true)

(def width 800)

(def height 500)

(defn sample [x y w h]
  (q/get-pixel x y w h))

#_(def dot-distance 120)
#_(def dot-size 70)
(def dot-distance 5)

(def dot-size 2)

(defn draw-dots! [rgba bounding-box angle width height center-x center-y dot-size]
  (let [rotation-matrix (hm/build-rotation center-x
                                           center-y
                                           (hm/clockwise-rotation angle))]

    (q/with-fill rgba
      (doseq [v (m/mmul bounding-box rotation-matrix)
              :let [x (m/mget v 0)
                    y (m/mget v 1)]
              :when (and (< x width)
                         (> x 0)
                         (< y height)
                         (> y 0))]
        (q/ellipse x y dot-size dot-size)))))

(defn sample-pixels [^processing.core.PImage image x y w h]
  (let [pixels ^ints (q/pixels (q/get-pixel image x y w h))
        wxh (* w h)
        color-totals (areduce pixels i ret
                              {:r 0
                               :g 0
                               :b 0}
                              (let [colors (aget pixels i)]
                                (-> ret
                                    (update :r #(+ % (q/red colors)))
                                    (update :g #(+ % (q/green colors)))
                                    (update :b #(+ % (q/blue colors))))))]
    (-> color-totals
        (update :r (fn [r] (/ r wxh)))
        (update :g (fn [g] (/ g wxh)))
        (update :b (fn [b] (/ b wxh))))))

(defn setup []
  (q/background 255)
  (q/no-loop)
  (q/set-state! :source-image
                (q/load-image "/Users/clark/Downloads/Awww.jpg")))

(defn draw []
  (println (sample-pixels (q/state :source-image) 40 40 10 10))

  #_(let [center-x (/ width 2)
          center-y (/ height 2)
          bounding-box (hm/bounding-box-dots width
                                             height
                                             center-x
                                             center-y
                                             dot-distance)
          cyan (future
                 (q/with-graphics ^PGraphicsSVG (q/create-graphics width height :svg "cyan-mm-small.svg")
                   (q/no-stroke)
                   (draw-dots! [10 120 200 190]
                               bounding-box
                               60
                               width
                               height
                               center-x
                               center-y
                               dot-size)))
          magenta (future
                    (q/with-graphics ^PGraphicsSVG (q/create-graphics width height :svg "magenta-mm.svg")
                      (q/no-stroke)
                      (draw-dots! [200 10 100 190]
                                  bounding-box
                                  45
                                  width
                                  height
                                  center-x
                                  center-y
                                  dot-size)))
          yellow (future
                   (q/with-graphics ^PGraphicsSVG (q/create-graphics width height :svg "yellow-mm.svg")
                     (q/no-stroke)
                     (draw-dots! [250 200 10 190]
                                 bounding-box
                                 75
                                 width
                                 height
                                 center-x
                                 center-y
                                 dot-size)))
          black (future
                  (q/with-graphics ^PGraphicsSVG (q/create-graphics width height :svg "black-mm.svg")
                    (q/no-stroke)
                    (draw-dots! [0 0 0 45]
                                bounding-box
                                110
                                width
                                height
                                center-x
                                center-y
                                dot-size)))
          all (future
                (q/with-graphics ^PGraphicsSVG (q/create-graphics width height :svg "all-mm-small.svg")
                  (q/no-stroke)
                ;; cyan
                  (draw-dots! [10 120 200 190]
                              bounding-box
                              60
                              width
                              height
                              center-x
                              center-y
                              dot-size)
                ;; magenta
                  (draw-dots! [200 10 100 190]
                              bounding-box
                              45
                              width
                              height
                              center-x
                              center-y
                              dot-size)
                ;; yellow
                  (draw-dots! [250 200 10 190]
                              bounding-box
                              75
                              width
                              height
                              center-x
                              center-y
                              dot-size)
                ;; black
                  (draw-dots! [0 0 0 45]
                              bounding-box
                              110
                              width
                              height
                              center-x
                              center-y
                              dot-size)))]
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
       ;; :renderer :svg
       ;; :output-file "out6.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
