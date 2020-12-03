(ns advent-of-code.dec-2020.day-01
  (:require [ysera.test :refer [is= deftest]]))


(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_01.txt")
       (clojure.string/split-lines)
       (map read-string)))


(defn get-numbers-that-sums-to-2020
  {:test (fn []
           (is= (get-numbers-that-sums-to-2020 [1721 979 366 299 675 1456])
                #{1721 299}))}
  [numbers]
  (loop [[n & ns] numbers]
    (if-let [on (some (fn [on] (when (= (+ n on) 2020) on)) ns)]
      (set [n on])
      (recur ns))))


(deftest puzzle-1a
         (is= (let [numbers (get-numbers-that-sums-to-2020 (get-puzzle-input))]
                (apply * numbers))
              744475))

(defn get-product-of-triple-summing-to-2020
  {:test (fn []
           (is= (get-product-of-triple-summing-to-2020 [1721 979 366 299 675 1456])
                241861950))}
  [numbers]
  (-> (for [n1 numbers
            n2 numbers
            n3 numbers
            :when (<= n1 n2 n3)
            :when (= (+ n1 n2 n3) 2020)]
        (* n1 n2 n3))
      (first)))

(deftest puzzle-1b
         (is= (get-product-of-triple-summing-to-2020 (get-puzzle-input))
              70276940))
