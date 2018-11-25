(ns halftone.matrix
  (:require [clojure.core.matrix :as m]
            [quil.core :as q]))

(set! *warn-on-reflection* true)

(m/set-current-implementation :vectorz)

(def xdim 1000)
(def ydim 1000)

(def angle 45)

(def theta (Math/toRadians angle))

;; coun

(def clockwise-rot (m/array [[(Math/cos theta)     (Math/sin theta) 0]
                             [(- (Math/sin theta)) (Math/cos theta) 0]
                             [0                    0                1]]))

(def ccw-rot (m/array [[(Math/cos theta) (- (Math/sin theta)) 0]
                       [(Math/sin theta) (Math/cos theta)     0]
                       [0                0                    1]]))

(defn translation [x y]
  (m/array [[1 0 x]
            [0 1 y]
            [0 0 1]]))

(defn build-rot [x y]
  (let [t1 (translation x y)
        t2 (translation (- x) (- y))
        rot clockwise-rot]

    (m/mmul t1 rot t2)))

(def rot-in-place (build-rot 500 500))

(def xys (m/array [40 40 1]))

(def inputm (time (m/array (for [^double y (range 300 740 40)
                            ^double x (range 300 740 40)]
                        [x y 1]))))

(def bounding-box (m/array (for [y (range (- 500 (/ 566 2))
                                          (+ 500 (/ 566 2))
                                          40)
                                 x (range (- 500 (/ 566 2))
                                          (+ 500 (/ 566 2))
                                          40)]
                             (m/array [x y 1]))))

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn draw []
  (q/no-stroke)
  (q/with-fill [0 0 0]
    (q/text (.toString ^Long angle) 40 40))

  ;; blue
  #_(q/with-fill [0 0 200]
      (doseq [[x y] (into [] (map (fn [v] (into [] v)) (m/mmul inputm rot-in-place #_inputm)))]
        (q/ellipse x y 10 10)))


  #_(m/mmul bounding-box rot-in-place)

  #_(m/shape rot-in-place)

  #_(m/mmul (take 3 bounding-box) (m/transpose rot-in-place))

  #_(q/with-fill [0 200 200]
    (time (doseq [[x y] (into [] (map (fn [v] (into [] v)) #_(time (m/mmul bounding-box (m/transpose rot-in-place)))
                                      (map (fn [v] (m/mmul rot-in-place v))
                                           bounding-box)))]

       (q/ellipse x y 10 10))))

  (q/with-fill [0 200 200]
    (time (doseq [v (m/mmul bounding-box (m/transpose rot-in-place))] ;; note that rot-in-place is transposed, not sure why it has to be that way

       (q/ellipse (m/mget v 0) ;; x
                  (m/mget v 1) ;; y
                  10 10))))

  ; red
  (q/with-fill [250 180 80]
    (doseq [[x y] (into [] (map (fn [v] (into [] v))
                                inputm))]
      (q/ellipse x y 10 10))))

#_(into [] (map (fn [v] (into [] v))
                inputm))

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
