(ns advent-of-code.dec-2022.day-02
  (:require [ysera.test :refer [is= deftest]]
            [clojure.string :refer [split split-lines]]))

; Done by my ten-year-old son Emil

(def input-data (->> (slurp "src/advent_of_code/dec_2022/day_02_input.txt")
                     (split-lines)))

(def score {"A X" 4
            "A Y" 8
            "A Z" 3
            "B X" 1
            "B Y" 5
            "B Z" 9
            "C X" 7
            "C Y" 2
            "C Z" 6})


(->> input-data
     (map score)
     (apply +))

; X - lose, Y - draw, Z - win

(def score2 {"A X" 3
             "A Y" 4
             "A Z" 8
             "B X" 1
             "B Y" 5
             "B Z" 9
             "C X" 2
             "C Y" 6
             "C Z" 7})

(->> input-data
     (map score2)
     (apply +))


