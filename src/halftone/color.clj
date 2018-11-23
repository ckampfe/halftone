(ns halftone.color)

(defn clamp [minimum maximum]
  (fn [n]
    (if (< n minimum)
      minimum
      (if (> n maximum)
        maximum
        n))))

(def clamp-to-one (clamp 0 1))

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
    {:c cyan
     :m magenta
     :y yellow
     :k black}))

(defn normalize-color-value-to-1 [n]
  (/ n 255))

(defn rgb->cmyk
  ([rgb] (normalized-rgb->cmyk (map normalize-color-value-to-1 rgb)))
  ([r g b] (rgb->cmyk [r g b])))

(comment
  (def yellowish-color [169 179 42])

  (def burnt-orange [178 107 62])

  (rgb->cmyk burnt-orange)

;; greenish
  (rgb->cmyk [13 107 62])

  (normalized-rgb->cmyk (map normalize-color-value-to-1 yellowish-color)))
