(ns advent-of-code.dec-2022.day-02
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]))

(def input-data (->> (slurp "src/advent_of_code/dec_2022/day_02_input.txt")
                     (clojure.string/split-lines)))

; A and X - Rock 1p
; B and Y - Paper 2p
; C and Z - Scissors 3p

(def score-1 {"A X" 4
              "A Y" 8
              "A Z" 3
              "B X" 1
              "B Y" 5
              "B Z" 9
              "C X" 7
              "C Y" 2
              "C Z" 6})

(->> input-data
     (map score-1)
     (reduce +))

; X - lose, Y - draw, Z - win

(def score-2 {"A X" 3                                       ; need Scissors
              "A Y" 4
              "A Z" 8                                       ; need Paper
              "B X" 1                                       ; need Rock
              "B Y" 5
              "B Z" 9                                       ; need Scissors
              "C X" 2                                       ; need Paper
              "C Y" 6
              "C Z" 7})

(->> input-data
     (map score-2)
     (reduce +))

