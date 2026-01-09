(ns advent-of-code.dec-2025.day-12
  (:require [clojure.edn :as edn]
            [clojure.string :as string]))

(def input (slurp "src/advent_of_code/dec_2025/day_12_input.txt"))

(comment
  (time (->> input
             (string/split-lines)
             (drop-while (fn [r] (not (re-find #"^\d+x\d+" r))))
             (reduce (fn [a shape]
                       (let [[x-dimension y-dimension] (->> (re-find #"(\d+)x(\d+):" shape)
                                                            (rest)
                                                            (map edn/read-string))
                             area (* x-dimension y-dimension)
                             ns (->> (re-seq #"\d+" shape)
                                     (drop 2)
                                     (map edn/read-string))]
                         (if (<= (* 9 (apply + ns))
                                 area)
                           (inc a)
                           a)))
                     0)))
  )

