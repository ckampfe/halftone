(ns halftone.matrix
  (:require [clojure.core.matrix :as m]
            [quil.core :as q]))

(m/set-current-implementation :vectorz)

(def angle 30)

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

    (time (m/mmul t1 rot t2))))

(def rot-in-place (build-rot 400 250))

(def xys (m/array [40 40 1]))

(def inputm (m/array (for [y (range 125 375 40)
                           x (range 275 525 40)]
                       [x y 1])))

(defn setup []
  (q/background 255)
  (q/no-loop))

(defn draw []
  (q/with-fill [0 0 0]
    (q/text (.toString angle) 40 40))

  ;; blue
  #_(q/with-fill [0 0 200]
    (doseq [[x y] (into [] (map (fn [v] (into [] v)) (m/mmul inputm rot-in-place #_inputm)))]
      (q/ellipse x y 10 10)))

  (q/with-fill [0 0 200]
    (doseq [[x y] (into [] (map (fn [v] (into [] v)) (time (map (fn [v]
                                                             (m/mmul rot-in-place v))
                                                           inputm))))]
      (q/ellipse x y 10 10)))

  ; red
  (q/with-fill [200 0 0]
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
       :size [800 500]
       ;; :renderer :svg
       ;; :output-file "out5.svg"
       :features [:no-bind-output]
)
     (catch Exception e
       (println e)))
