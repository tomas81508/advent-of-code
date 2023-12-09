(ns advent-of-code.dec-2023.day-09
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_09_input.txt")
                (split-lines)
                (map (fn [r] (->> (split r #" ")
                                  (map read-string))))))

(def test-input [[0 3 6 9 12 15]
                 [1 3 6 10 15 21]
                 [10 13 16 21 30 45]])

(defn create-difference-seq
  {:test (fn []
           (is= (create-difference-seq [1 3 6 10 15 21])
                [2 3 4 5 6]))}
  [s]
  (->> s
       (partition 2 1)
       (map (fn [[a b]] (- b a)))))

(defn generate-last-values
  {:test (fn []
           (is= (generate-last-values [10 13 16 21 30 45])
                [2 6 15 45]))}
  [s]
  (loop [s s
         last-elements (list)]
    (if (every? zero? s)
      last-elements
      (recur (create-difference-seq s) (conj last-elements (last s))))))

(defn next-history-value
  {:test (fn []
           (is= (next-history-value [10 13 16 21 30 45])
                68))}
  [s]
  (->> (generate-last-values s)
       (reduce +)))

(deftest puzzle-a
  (is= (time (->> input
                  (map next-history-value)
                  (reduce +)))
       ; "Elapsed time: 28.773848 msecs"
       1684566095))

; part 2

(defn generate-first-values
  {:test (fn []
           (is= (generate-first-values [1 3 6 10 15 21])
                [1 2 1])
           (is= (generate-first-values [10 13 16 21 30 45])
                [10 3 0 2]))}
  [s]
  (loop [s s
         last-elements []]
    (if (every? zero? s)
      last-elements
      (recur (create-difference-seq s) (conj last-elements (first s))))))

(defn precious-history-value
  {:test (fn []
           (is= (precious-history-value [0 3 6 9 12 15])
                -3)
           (is= (precious-history-value [1 3 6 10 15 21])
                0)
           (is= (precious-history-value [10 13 16 21 30 45])
                5))}
  [s]
  (->> (generate-first-values s)
       (reverse)
       (reduce (fn [a v] (- v a)))))

(deftest puzzle-b
  (is= (time (->> input
                  (map precious-history-value)
                  (reduce +)))
       ; "Elapsed time: 24.773848 msecs"
       1136))
