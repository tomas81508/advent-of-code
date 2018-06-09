(ns advent-of-code.dec-2016.day-03
  (:require [ysera.test :refer [deftest is is-not is=]]
            [clojure.string :as string]))

; --- Day 3: Squares With Three Sides ---
;
; Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this
; part of Easter Bunny HQ. This must be a graphic design department; the walls are covered in specifications for
; triangles.
;
; Or are they?
;
; The design document gives the side lengths of each triangle it describes, but... 5 10 25? Some of these aren't
; triangles. You can't help but mark the impossible ones.
;
; In a valid triangle, the sum of any two sides must be larger than the remaining side. For example, the "triangle"
; given above is impossible, because 5 + 10 is not larger than 25.
;
; In your puzzle input, how many of the listed triangles are possible?

(defn sides-of-a-triangle?
  {:test (fn []
           (is (sides-of-a-triangle? 3 4 5))
           (is-not (sides-of-a-triangle? 3 10 4))
           (is-not (sides-of-a-triangle? 10 4 3))
           (is-not (sides-of-a-triangle? 2 1 3)))}
  [a b c]
  (let [[k1 k2 h] (sort [a b c])]
    (> (+ k1 k2) h)))

(def input-data (as-> (slurp "src/advent_of_code/dec_2016/day_03_input.txt") $
                      (string/split $ #"[ ]+|\n")
                      (remove empty? $)
                      (map read-string $)))

(deftest puzzle-a
         (is= (->> input-data
                   (partition 3)
                   (filter (fn [triple] (apply sides-of-a-triangle? triple)))
                   (count))
              1050))

(deftest puzzle-b
         (is= (->> input-data
                   (partition 3)
                   (apply map list)
                   (flatten)
                   (partition 3)
                   (filter (fn [triple] (apply sides-of-a-triangle? triple)))
                   (count))
              1921))