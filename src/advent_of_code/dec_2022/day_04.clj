(ns advent-of-code.dec-2022.day-04
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(def input-data (->> (slurp "src/advent_of_code/dec_2022/day_04_input.txt")
                     (clojure.string/split-lines)))

(def pattern #"(\d+)-(\d+),(\d+)-(\d+)")

(defn compare?
  [work]
  (let [[x1 x2 y1 y2] (->> (re-find pattern work)
                           (drop 1)
                           (map read-string))]
    (or (and (<= x1 y1)
             (>= x2 y2))
        (and (<= y1 x1)
             (>= y2 x2)))))

(->> input-data
     (map compare?)
     (filter true?)
     (count))

(defn compare2?
  {:test (fn []
           (is (compare2? "2-5,3-7"))
           (is (compare2? "4-10,3-7"))
           (is-not (compare2? "4-10,12-17"))
           (is-not (compare2? "24-210,12-17")))}
  [work]
  (let [[x1 x2 y1 y2] (->> (re-find pattern work)
                           (drop 1)
                           (map read-string))]
    (or (<= x1 y1 x2)
        (<= y1 x1 y2))))

(->> input-data
     (map compare2?)
     (filter true?)
     (count))
