(ns advent-of-code.dec-2015.day-01
  (:require [ysera.test :refer [is= deftest]]))

(defn get-puzzle-input []
  (slurp "src/advent_of_code/dec_2015/day_01_input.txt"))

(deftest puzzle-a
         (is= (->> (get-puzzle-input)
                   (map (fn [x] (if (= x \() 1 -1)))
                   (apply +))
              74))

(deftest puzzle-b
         (is= (->> (slurp "src/advent_of_code/dec_2015/day_01_input.txt")
                   (map-indexed (fn [index x] [(inc index) x]))
                   (reduce (fn [a v]
                             (if (= (second v) \()
                               (inc a)
                               (if (= a 0)
                                 (reduced (first v))
                                 (dec a))))
                           0))
              1795))
