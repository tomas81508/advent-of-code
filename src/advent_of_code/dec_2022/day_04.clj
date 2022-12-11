(ns advent-of-code.dec-2022.day-04
  (:require [ysera.test :refer [is is-not is= deftest]]))

; Done by my ten-year-old son Emil

(def input-data (->> (slurp "src/advent_of_code/dec_2022/day_04_input.txt")
                     (clojure.string/split-lines)))

(def work "3-87,4-4")

(def pattern #"(\d+)-(\d+),(\d+)-(\d+)")

(re-find pattern work)

(def work2 "28-96,28-28")
(def work3 "28-30,25-40")

(defn compare?
  [work]
  (let [[x1 x2 y1 y2] (->> (re-find pattern work)
                           (drop 1)
                           (map read-string))]
    (or (and (<= x1 y1)
             (>= x2 y2))
        (and (<= y1 x1)
             (>= y2 x2)))))

(compare? work2)
(compare? work3)

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
