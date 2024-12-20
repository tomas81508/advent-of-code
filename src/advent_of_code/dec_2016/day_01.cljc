(ns advent-of-code.dec-2016.day-01
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.collections :refer [seq-contains?]]
            [clojure.string :refer [split]]))

; --- Day 1: No Time for a Taxicab ---
;
; You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can
; get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time
; to work them out further.
;
; The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then,
; follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of
; blocks, ending at a new intersection.
;
; There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the
; destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the
; destination?
;
; For example:
;
; Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
; R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
; R5, L5, R5, R3 leaves you 12 blocks away.
; How many blocks away is Easter Bunny HQ?

(defn- get-new-direction
  {:test (fn []
           (is= (get-new-direction [0 1] "L") [-1 0])
           (is= (get-new-direction [-1 0] "L") [0 -1])
           (is= (get-new-direction [0 -1] "L") [1 0])
           (is= (get-new-direction [1 0] "L") [0 1])
           (is= (get-new-direction [0 1] "R") [1 0])
           (is= (get-new-direction [1 0] "R") [0 -1])
           (is= (get-new-direction [0 -1] "R") [-1 0])
           (is= (get-new-direction [-1 0] "R") [0 1]))}
  [old-direction turn]
  {:pre [(or (= turn "L") (= turn "R"))]}
  (if (= turn "L")
    [(- (second old-direction)) (first old-direction)]
    [(second old-direction) (- (first old-direction))]))

(defn abs
  {:test (fn []
           (is= (abs 5) 5)
           (is= (abs 0) 0)
           (is= (abs -3) 3))}
  [value]
  (if (pos? value) value (- value)))

(defn- get-turn
  {:test (fn []
           (is= (get-turn "R33") "R")
           (is= (get-turn "L1") "L"))}
  [directive]
  (subs directive 0 1))

(defn- get-blocks
  {:test (fn []
           (is= (get-blocks "R33") 33)
           (is= (get-blocks "L3") 3))}
  [directive]
  (read-string (subs directive 1)))

(defn distance
  {:test (fn []
           (is= (distance 4 -3) 7)
           (is= (distance 4 5) 9)
           (is= (distance -4 5) 9))}
  [& xs]
  (->> (map abs xs)
       (apply +)))

(defn taxicab-geometry-distance
  "Given directives, the taxicab distance is calculated."
  {:test (fn []
           (is= (taxicab-geometry-distance "R2" "L3") 5)
           (is= (taxicab-geometry-distance "R2" "R2" "R2") 2)
           (is= (taxicab-geometry-distance "R5" "L5" "R5" "R3") 12)
           (is= (taxicab-geometry-distance "R5" "L15") 20))}
  [& directives]
  (->> (reduce (fn [[coordinate direction] directive]
                 (let [; Get the new face direction, for instance [-1 0]
                       new-direction (get-new-direction direction (get-turn directive))
                       ; Get the vector how to walk, for instance [-5 0]
                       direction-walk (map (fn [v] (* (get-blocks directive) v)) new-direction)]
                   [(map + coordinate direction-walk) new-direction]))
               [[0 0] [0 1]]
               directives)
       (first)
       (apply distance)))

; My input

(def input-data
  (-> (slurp "src/advent_of_code/dec_2016/day_01_input.txt")
      (split #", ")))

(deftest puzzle-a
  (is= (->> input-data
            (apply taxicab-geometry-distance))
       287))

; --- Part Two ---

; Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the
; first location you visit twice.
;
; For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.
;
; How many blocks away is the first location you visit twice?

(defn first-place-to-visit-twice
  {:test (fn []
           (is= (first-place-to-visit-twice "R8" "R4" "R4" "R8")
                [4 0])
           (is= (first-place-to-visit-twice "R2" "L2")
                nil))}
  [& directives]
  (loop [visited-coordinates [(list 0 0)]
         face-direction [0 1]
         directive-index 0
         block-index 0]
    (let [current-coordinates (last visited-coordinates)
          directive (nth directives directive-index)
          new-face-direction (if (= block-index 0)
                               (get-new-direction face-direction (get-turn directive))
                               face-direction)
          number-of-blocks (get-blocks directive)
          new-coordinates (map + current-coordinates new-face-direction)]
      (cond (seq-contains? visited-coordinates new-coordinates)
            new-coordinates

            (< block-index (dec number-of-blocks))
            (recur (conj visited-coordinates new-coordinates) new-face-direction directive-index (inc block-index))

            (< directive-index (dec (count directives)))
            (recur (conj visited-coordinates new-coordinates) new-face-direction (inc directive-index) 0)))))

(deftest puzzle-b
  (is= (->> input-data
            (apply first-place-to-visit-twice)
            (apply distance))
       133))






















