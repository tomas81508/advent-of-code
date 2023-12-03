(ns advent-of-code.dec-2023.day-01
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_01_input.txt")
                (clojure.string/split-lines)))

(defn find-digit [l]
  (read-string (re-find #"[0-9]" l)))

(defn find-number [l]
  (let [d1 (find-digit l)
        d2 (find-digit (apply str (reverse l)))]
    (read-string (str d1 d2))))

(deftest puzzle-a
  (is= (->> input
            (map find-number)
            (reduce +))
       54953))

; part 2

(def word->number {"one" "o1e" "two" "t2o" "three" "t3e" "four" "f4r" "five" "f5e"
                   "six" "s6x" "seven" "s7n" "eight" "e8t" "nine" "n9e"})

(defn words->numbers
  [l]
  (let [word-number (re-find #"one|two|three|four|five|six|seven|eight|nine" l)]
    (if word-number
      (recur (clojure.string/replace l (re-pattern word-number) word->number))
      l)))

(defn find-number2 [l]
  (-> (words->numbers l)
      (find-number)))

(deftest puzzle-b
  (is= (->> input
            (map find-number2)
            (reduce +))
       53868))
