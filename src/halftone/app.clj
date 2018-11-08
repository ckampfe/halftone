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
  (let [adj (+ 90 (* -1 angle))]
    adj
    #_(if (<= adj -90)
        (+ 180 adj)
        adj)))

(defn to-radians [degrees]
  (java.lang.Math/toRadians degrees))

(defn compute-rise-and-run [angle-b side-a]
  (let [angle-a 90
        angle-c (- 180 (+ angle-a angle-b))
        a-over-sin-side-a (/ side-a (java.lang.Math/sin ^double (to-radians angle-a)))
        side-b (* (java.lang.Math/sin (to-radians angle-b))
                  a-over-sin-side-a)
        side-c (* (java.lang.Math/sin (to-radians angle-c))
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

(defn true-start-pos [angle])

(defn draw-dots [rgba angle dot-size dot-distance width height & opts]
  (let [start-pos -33
        angle-2 (- angle 90)
        angle-3 (+ angle 90)]
      (q/with-fill rgba
        (doseq [starter (take 100 (dotvector-seq2 (if (> angle-3 180)
                                                    (+ width start-pos)
                                                    start-pos
                                                    )
                                                  start-pos angle-2 dot-distance))
             {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) angle dot-distance))]
       (q/ellipse x y dot-size dot-size))

        (doseq [starter (take 100 (drop 1 (dotvector-seq2 (if (> angle-3 180)
                                                            (+ width start-pos)
                                                            start-pos
                                                            )
                                                          start-pos angle-3 dot-distance)))
             {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) angle dot-distance))]
       (q/ellipse x y dot-size dot-size)))))

(defn setup []
  ;; (try
      (q/background 255)
      (q/no-loop)
      #_(q/fill 100 100 100)
      ;;(catch Exception e (pr-str (.toString e)))
      ;; )
  )

(defn draw []

  #_(q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "cyan.svg")
    (q/no-stroke)
    ;; CYAN
    (q/with-fill [10 120 200 190]
      (doseq [starter (take 100 (dotvector-seq2 -33 -33 -30 dot-distance))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 60 dot-distance))]
        (q/ellipse x y dot-size dot-size))

      (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 150 dot-distance)))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 60 dot-distance))]
        (q/ellipse x y dot-size dot-size)))

    )
      ;; CYAN
  (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "cyan2.svg")
    (q/no-stroke)
    (draw-dots [10 120 200 190] 60 dot-size dot-distance 800 500))
      #_(q/with-fill [10 120 200 190]
        (doseq [starter (take 100 (dotvector-seq2 -33 -33 -30 dot-distance))
                {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 60 dot-distance))]
          (q/ellipse x y dot-size dot-size))

        (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 150 dot-distance)))
                {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 60 dot-distance))]
          (q/ellipse x y dot-size dot-size)))

  ;; MAGENTA
  (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "magenta2.svg")
    (q/no-stroke)
    (draw-dots [200 10 100 190] 45 dot-size dot-distance 800 500))
  #_(q/with-graphics (q/create-graphics 800 500 :svg "magenta.svg")
    (q/no-stroke)
    (q/with-fill [200 10 100 190]
      (doseq [starter (take 100 (dotvector-seq2 -33 -33 -45 dot-distance))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 45 dot-distance))]
        (q/ellipse x y dot-size dot-size))

      (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 135 dot-distance)))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 45 dot-distance))]
        (q/ellipse x y dot-size dot-size))))

  ;; YELLOW
  (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "yellow2.svg")
    (q/no-stroke)
    (draw-dots [250 200 10 190] 75 dot-size dot-distance 800 500))
  #_(q/with-graphics (q/create-graphics 800 500 :svg "yellow.svg")
    (q/no-stroke)
    (q/with-fill [250 200 10 190]
      (doseq [starter (take 100 (dotvector-seq2 -33 -33 -15 dot-distance))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 75 dot-distance))]
        (q/ellipse x y dot-size dot-size))

      (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 165 dot-distance)))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 75 dot-distance))]
        (q/ellipse x y dot-size dot-size))))

  ;; BLACK/KEY
  (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "black2.svg")
    (q/no-stroke)
    (draw-dots [0 0 0 45] 110 dot-size dot-distance 800 500)
    #_(q/with-fill [0 0 0 45]
      (doseq [starter (take 100 (dotvector-seq2 833 #_-33 -33 20 dot-distance))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 110 dot-distance))]
        (q/ellipse x y dot-size dot-size))

      (doseq [starter (take 100 (drop 1 (dotvector-seq2 833 #_-33 -33 200 dot-distance)))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 110 dot-distance))]
        (q/ellipse x y dot-size dot-size))))

  (q/with-graphics ^PGraphicsSVG (q/create-graphics 800 500 :svg "all.svg")
    (q/no-stroke)
    ;; cyan
    (draw-dots [10 120 200 190] 60 dot-size dot-distance 800 500)
    ;; magenta
    (draw-dots [200 10 100 190] 45 dot-size dot-distance 800 500)
    ;; yellow
    (draw-dots [250 200 10 190] 75 dot-size dot-distance 800 500)
    ;; black
    (draw-dots [0 0 0 45] 110 dot-size dot-distance 800 500)
    )
  )

(try (q/defsketch example
       :title "halftone"
       :settings #(q/smooth 8)
       :setup setup
       :draw draw
       :size [800 500]
       :renderer :svg
       :output-file "out5.svg")
     (catch Exception e
       (println e)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
