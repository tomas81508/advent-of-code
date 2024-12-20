(ns advent-of-code.dec-2021.day-01
  (:require [advent-of-code.test :refer [is=]]
                       [clojure.test :refer [deftest]]))

(def input-data (->> (slurp "src/advent_of_code/dec_2021/day_01_input.txt")
                     (clojure.string/split-lines)
                     (map read-string)))

(defn count-depth-measurement-increases
  {:test (fn []
           (is= (count-depth-measurement-increases [199 200 208 210 200 207 240 269 260 263])
                7))}
  [data]
  (->> data
       (partition 2 1)
       (map (fn [[x1 x2]] (- x2 x1)))
       (filter pos?)
       (count)))

(deftest puzzle-a
         (is= (time (count-depth-measurement-increases input-data))
              ; "Elapsed time: 1.981313 msecs"
              1602))

(defn count-depth-measurement-increases-2
  {:test (fn []
           (is= (count-depth-measurement-increases-2 [199 200 208 210 200 207 240 269 260 263])
                5))}
  [data]
  (->> data
       (partition 3 1)
       (map (fn [ns] (apply + ns)))
       (count-depth-measurement-increases)))

(deftest puzzle-b
         (is= (time (count-depth-measurement-increases-2 input-data))
              ; "Elapsed time: 3.153576 msecs"
              1633))