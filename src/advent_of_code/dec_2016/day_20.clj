(ns advent-of-code.dec-2016.day-20
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

(def input (->> (slurp "src/advent_of_code/dec_2016/day_20_input.txt")
                (string/split-lines)
                (map (fn [row] (->> (re-seq #"\d+" row)
                                    (map read-string))))))

(defn blacklisted
  {:test (fn []
           (is (blacklisted [[0 7] [9 14] [3 10] [16 19]] 9))
           (is-not (blacklisted [[0 7] [9 14] [3 10] [16 19]] 15)))}
  [input n]
  (->> input
       (some (fn [[a b]] (<= a n b)))))

(defn solve-a
  []
  (->> input
       (map (fn [[_ b]] (inc b)))
       (map (fn [n] (when-not (blacklisted input n) n)))
       (remove nil?)
       (apply min)))

(defn interval-difference
  {:test (fn []
           (is= (interval-difference [[5 10]] [7 11]) [[5 6]])
           (is= (interval-difference [[5 10]] [2 3]) [[5 10]])
           (is= (interval-difference [[5 10]] [2 8]) [[9 10]])
           (is= (interval-difference [[5 10]] [12 18]) [[5 10]])
           (is= (interval-difference [[5 10]] [8 18]) [[5 7]])
           (is= (interval-difference [[5 10]] [3 18]) [])
           (is= (interval-difference [[5 10]] [7 9]) [[5 6] [10 10]]))}
  [u [a b]]
  (->> u
       (reduce (fn [acc [ua ub :as u]]
                 (cond (< ub a) (conj acc u)
                       (> ua b) (conj acc u)
                       (and (>= a ua) (<= b ub))
                       (conj acc [ua (dec a)] [(inc b) ub])
                       (>= a ua) (conj acc [ua (dec a)])
                       (<= b ub) (conj acc [(inc b) ub])
                       :else acc))
               [])
       (remove (fn [[a b]] (< b a)))))

(defn whitelisted
  {:test (fn []
           (is= (whitelisted [[0 7] [9 14] [3 10] [16 19]] 23)
                [[15 15] [20 23]]))}
  [input max-n]
  (->> input
       (reduce interval-difference
               [[0 max-n]])))

(defn count-whitelisted
  {:test (fn []
           (is= (count-whitelisted [[0 7] [9 14] [3 10] [16 19]] 23)
                5))}
  [input max-n]
  (->> (whitelisted input max-n)
       (map (fn [[a b]] (inc (- b a))))
       (reduce +)))

(defn solve-b [] (count-whitelisted input 4294967295))






