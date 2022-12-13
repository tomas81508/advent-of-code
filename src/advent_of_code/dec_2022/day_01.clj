(ns advent-of-code.dec-2022.day-01
  (:require [clojure.test :refer [deftest]]
            [ysera.test :refer [is=]]))

(def input-data (->> (slurp "src/advent_of_code/dec_2022/day_01_input.txt")
                     (clojure.string/split-lines)))

(deftest puzzle-a
  (is= (as-> (slurp "src/advent_of_code/dec_2022/day_01_input.txt") $
             (clojure.string/split $ #"\n\n")
             (map (fn [l]
                    (->> (clojure.string/split l #"\n")
                         (map read-string)
                         (reduce +)))
                  $)
             (reduce max $))
       69289))

(deftest puzzle-b
  (is= (as-> (slurp "src/advent_of_code/dec_2022/day_01_input.txt") $
             (clojure.string/split $ #"\n\n")
             (map (fn [l]
                    (->> (clojure.string/split l #"\n")
                         (map read-string)
                         (reduce +)))
                  $)
             (sort > $)
             (take 3 $)
             (reduce + $))
       205615))
