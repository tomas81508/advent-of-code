(ns advent-of-code.dec-2019.day-01
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.math :refer [floor]]))


(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2019/day_01.txt")
       (clojure.string/split-lines)
       (map read-string)))


(defn required-fuel
  {:test (fn []
           (is= (required-fuel 12) 2)
           (is= (required-fuel 14) 2)
           (is= (required-fuel 1969) 654)
           (is= (required-fuel 100756) 33583))}
  [mass]
  (-> mass
      (/ 3)
      (floor)
      (- 2)))

(deftest puzzle-1a
  (is= (->> (get-puzzle-input)
            (map required-fuel)
            (apply +))
       3296560))

(defn total-required-fuel
  {:test (fn []
           (is= (total-required-fuel 12) 2)
           (is= (total-required-fuel 1969) 966)
           (is= (total-required-fuel 100756) 50346))}
  [mass]
  (loop [mass mass
         total-fuel 0]
    (let [fuel (required-fuel mass)]
      (if (pos? fuel)
        (recur fuel (+ fuel total-fuel))
        total-fuel))))

(deftest puzzle-1a
  (is= (->> (get-puzzle-input)
            (map total-required-fuel)
            (apply +))
       4941976))
