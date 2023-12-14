(ns advent-of-code.dec-2023.day-11
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]
            [clojure.math.combinatorics :as combinatorics]))


(def input (->> (slurp "src/advent_of_code/dec_2023/day_11_input.txt")
                (split-lines)))

(def test-input ["...#......"
                 ".......#.."
                 "#........."
                 ".........."
                 "......#..."
                 ".#........"
                 ".........#"
                 ".........."
                 ".......#.."
                 "#...#....."])

(defn no-galaxy-rows
  {:test (fn []
           (is= (no-galaxy-rows test-input)
                [3 7]))}
  [input]
  (->> input
       (map-indexed (fn [index row] [index row]))
       (filter (fn [[_ row]] (every? (fn [c] (= c \.)) row)))
       (map first)))

(defn no-galaxy-columns
  {:test (fn []
           (is= (no-galaxy-columns test-input)
                [2 5 8]))}
  [input]
  (reduce (fn [a i]
            (if (->> input
                     (map (fn [row] (get row i)))
                     (every? (fn [c] (= c \.))))
              (conj a i)
              a))
          []
          (range (count (first input)))))


(defn create-extended-state
  {:test (fn []
           (is= (create-extended-state test-input 2)
                #{[4 0] [9 1] [0 2] [8 5] [1 6] [12 7] [9 10] [5 11] [0 11]}))}
  [input factor]
  (let [ngr (no-galaxy-rows input)
        ngc (no-galaxy-columns input)
        size-x (count (first input))
        size-y (count input)]
    (reduce (fn [a y]
              (reduce (fn [a x]
                        (if (= (get-in input [y x]) \#)
                          (let [mod-x (+ x (* (dec factor) (count (filter (fn [p] (< p x)) ngc))))
                                mod-y (+ y (* (dec factor) (count (filter (fn [p] (< p y)) ngr))))]
                            (conj a [mod-x mod-y]))
                          a))
                      a
                      (range size-x)))
            #{}
            (range size-y))))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [-1 4] [2 2]) 5))}
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (reduce +)))

(defn calculate-distances
  {:test (fn []
           (is= (calculate-distances test-input 2) 374))}
  [input factor]
  (as-> (create-extended-state input factor) $
        (combinatorics/combinations $ 2)
        (map (fn [coordinates] (apply manhattan-distance coordinates)) $)
        (reduce + $)))

(deftest puzzle-a
  (is= (time (calculate-distances input 2))
       ; "Elapsed time: 122.110425 msecs"
       10173804))

(deftest example-10-times
  (is= (calculate-distances test-input 10)
       1030))

(deftest puzzle-b
  (is= (time (calculate-distances input 1000000))
       ; "Elapsed time: 126.994404 msecs"
       634324905172))


















