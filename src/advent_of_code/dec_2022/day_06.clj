(ns advent-of-code.dec-2022.day-06
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]))

(def input (slurp "src/advent_of_code/dec_2022/day_06_input.txt"))

(defn find-marker
  {:test (fn []
           (is= (find-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4) 7)
           (is= (find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 4) 5)
           (is= (find-marker "nppdvjthqldpwncqszvftbrmjlhg" 4) 6)
           (is= (find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 4) 10)
           (is= (find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 4) 11)
           (is= (find-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14) 19)
           (is= (find-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 14) 23)
           (is= (find-marker "nppdvjthqldpwncqszvftbrmjlhg" 14) 23)
           (is= (find-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14) 29)
           (is= (find-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 14) 26))}
  [input n]
  (->> input
       (partition n 1)
       (map-indexed (fn [index characters] [index (set characters)]))
       (filter (fn [[_ characters]] (= (count characters) n)))
       (ffirst)
       (+ n)))

(deftest puzzle-a
  ; "Elapsed time: 2.669118 msecs"
  (is= (time (find-marker input 4))
       1235))

(deftest puzzle-b
  ; "Elapsed time: 13.66514 msecs"
  (is= (time (find-marker input 14))
       3051))


