(ns halftone.matrix
  (:require [clojure.core.matrix :as m]
            [quil.core :as q]))

(set! *warn-on-reflection* true)

(m/set-current-implementation :vectorz)

(def xdim 1000)
(def ydim 1000)

(defn counter-clockwise-rotation [^double angle-degrees]
  (let [angle-radians (Math/toRadians angle-degrees)]
    (m/array [[(Math/cos angle-radians)     (Math/sin angle-radians) 0]
              [(- (Math/sin angle-radians)) (Math/cos angle-radians) 0]
              [0                            0                        1]])))

(defn clockwise-rotation [^double angle-degrees]
  (let [angle-radians (Math/toRadians angle-degrees)]
    (m/array [[(Math/cos angle-radians) (- (Math/sin angle-radians)) 0]
              [(Math/sin angle-radians) (Math/cos angle-radians)     0]
              [0                        0                            1]])))

(defn translation [x y]
  (m/array [[1 0 x]
            [0 1 y]
            [0 0 1]]))

(defn build-rotation
  "Build a rotation, that when applied to another matrix,
  will rotate that matrix around a given point (x,y)"
  [center-x center-y rotation-matrix]
  (let [translation-1 (translation center-x center-y)
        translation-2 (translation (- center-x) (- center-y))]
    (m/transpose (m/mmul translation-1 rotation-matrix translation-2))))

(defn bounding-box-side
  "compute the side length of the bounding box"
  [^double source-width ^double source-height]
  (+ (/ source-width (Math/sqrt 2))
     (/ source-height (Math/sqrt 2))))

(defn bounding-box-dots [source-width source-height center-x center-y dot-spacing]
  "source-width and source-height are the box for which we are computing the bounding box.
   center-x and y is the xy point that represents the center of the source box.
   dot spacing is the space between dots, in pixels."
  (let [bb-side (bounding-box-side source-width source-height)
        bb-side-over-2 (/ bb-side 2)]
    (m/array (for [y (range (- center-y bb-side-over-2)
                            (+ center-y bb-side-over-2)
                            dot-spacing)
                   x (range (- center-x bb-side-over-2)
                            (+ center-x bb-side-over-2)
                            dot-spacing)]
               (m/array [x y 1])))))

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn draw []

  (q/no-stroke)

  (let [angle 10 ;; degrees
        center-x 500
        center-y 500
        bounding-box (bounding-box-dots 400 400 500 500 40)
        rot-in-place (build-rotation center-x center-y (counter-clockwise-rotation angle))
        inputm (m/array (for [y (range 300 740 40)
                              x (range 300 740 40)]
                          [x y 1]))]

    (q/with-fill [0 0 0]
      (q/text (str angle) 40 40))

    (q/with-fill [0 200 200]
      (time (doseq [v (m/mmul bounding-box rot-in-place)]
              (q/ellipse (m/mget v 0) ;; x
                         (m/mget v 1) ;; y
                         10 10))))

    ;; red
    (q/with-fill [250 180 80]
      (doseq [[x y] (into [] (map (fn [v] (into [] v))
                                  inputm))]
        (q/ellipse x y 10 10)))))

(try (q/defsketch example
       :title "m"
       :settings #(q/smooth 1)
       :setup setup
       :draw draw
       :size [xdim ydim]
       ;; :renderer :svg
       ;; :output-file "out5.svg"
       :features [:no-bind-output])
     (catch Exception e
       (println e)))
