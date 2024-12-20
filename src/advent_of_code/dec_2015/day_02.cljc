(ns advent-of-code.dec-2015.day-02
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :refer [split-lines]]))

(defn get-puzzle-input []
  (slurp "src/advent_of_code/dec_2015/day_02_input.txt"))

(def pattern (re-pattern "([\\d]+)x([\\d]+)x([\\d]+)"))

(defn calculate-paper
  {:test (fn []
           (is= (calculate-paper 2 3 4)
                58)
           (is= (calculate-paper 1 1 10)
                43))}
  [width height length]
  (->> [(* width height) (* width length) (* height length)]
       (sort)
       (map (fn [n a] (* n a)) [3 2 2])
       (apply +)))

(deftest puzzle-a
  (is= (->> (get-puzzle-input)
            (split-lines)
            (map (fn [w-h-l]
                   (let [dimensions (re-matches pattern w-h-l)]
                     (->> (map read-string (rest dimensions))
                          (apply calculate-paper)))))
            (reduce +))
       1598415))

(defn calculate-ribbon
  {:test (fn []
           (is= (calculate-ribbon 2 3 4)
                34)
           (is= (calculate-ribbon 1 1 10)
                14))}
  [width height length]
  (+ (->> [width height length]
          (sort)
          (drop-last)
          (map (fn [n l] (* n l)) [2 2])
          (apply +))
     (* width height length)))

(deftest puzzle-b
  (is= (->> (get-puzzle-input)
            (split-lines)
            (map (fn [w-h-l]
                   (let [dimensions (re-matches pattern w-h-l)]
                     (->> (map read-string (rest dimensions))
                          (apply calculate-ribbon)))))
            (reduce +))
       3812909))

