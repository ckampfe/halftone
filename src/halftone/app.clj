(ns halftone.app
  (:require [quil.core :as q])
  (:gen-class))

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

(defn determine-sheet-side [angle]
  (if (> angle 90)
    :right
    :left))

(defn compute-rise-and-run [angle-b side-a]
  (let [angle-a 90
        angle-c (- 180 (+ angle-a angle-b))
        a-over-sin-side-a (/ side-a (java.lang.Math/sin (to-radians angle-a)))
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

(defn setup []
  (try
    (q/background 255)
    (q/no-stroke)
    #_(q/fill 100 100 100)

    ;; CYAN
    (q/with-fill [10 120 200 190]
      (doseq [starter (take 100 (dotvector-seq2 -33 -33 -30 dot-distance))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 60 dot-distance))]
        (q/ellipse x y dot-size dot-size))

      (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 150 dot-distance)))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 60 dot-distance))]
        (q/ellipse x y dot-size dot-size)))

    ;; MAGENTA
    (q/with-fill [200 10 100 190]
      (doseq [starter (take 100 (dotvector-seq2 -33 -33 -45 dot-distance))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 45 dot-distance))]
        (q/ellipse x y dot-size dot-size))

      (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 135 dot-distance)))
              {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 45 dot-distance))]
        (q/ellipse x y dot-size dot-size)))

      ;; YELLOW
      (q/with-fill [250 200 10 190]
        (doseq [starter (take 100 (dotvector-seq2 -33 -33 -15 dot-distance))
                {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 75 dot-distance))]
          (q/ellipse x y dot-size dot-size))

        (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 165 dot-distance)))
                {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 75 dot-distance))]
          (q/ellipse x y dot-size dot-size)))

      ;; BLACK/KEY
      (q/with-fill [0 0 0 45]
        (doseq [starter (take 100 (dotvector-seq2 -33 -33 20 dot-distance))
                {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 110 dot-distance))]
          (q/ellipse x y dot-size dot-size))

        (doseq [starter (take 100 (drop 1 (dotvector-seq2 -33 -33 200 dot-distance)))
                {:keys [x y]} (take 200 (dotvector-seq (:x starter) (:y starter) 110 dot-distance))]
          (q/ellipse x y dot-size dot-size)))

    (catch Exception e (pr-str (.toString e)))))

(try (q/defsketch example
       :title "halftone"
       :settings #(q/smooth 8)
       :setup setup
       :size [800 500])
     (catch Exception e
       (println e)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
