(ns advent-of-code.dec-2022.day-01
  (:require [ysera.test :refer [is= deftest]]
            [clojure.string :refer [split]]))

; Done by my ten-year-old son Emil

(def input-data (slurp "src/advent_of_code/dec_2022/day_01_input.txt"))

(split "EmilSaraSofia" #"S")

(def elf "13779\n15932\n16875\n2148")

(defn apply-calories
  [elf]
  (->> (split elf #"\n")
       (map read-string)
       (apply +)))

(->> (split input-data #"\n\n")
     (map apply-calories)
     (sort >)
     (take 3)
     (apply +))




