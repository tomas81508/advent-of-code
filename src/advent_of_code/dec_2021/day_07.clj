(ns advent-of-code.dec-2021.day-07
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def test-input "16,1,2,0,4,2,7,1,2,14")

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_07_input.txt"))

(defn distance-fn
  [^long x ^long y]
  (Math/abs (- x y)))

(defn calculate-fuel
  [d]
  (/ (* d (+ d 1)) 2))

(def calculate-fuel (memoize calculate-fuel))

(defn distance-2-fn
  {:test (fn []
           (is= (distance-2-fn 0 1) 1)
           (is= (distance-2-fn 0 2) 3)
           (is= (distance-2-fn 0 3) 6)
           (is= (distance-2-fn 0 4) 10))}
  [x y]
  (let [original-distance (distance-fn x y)]
    (calculate-fuel original-distance)))

(defn align-positions
  {:test (fn []
           (is= (align-positions test-input distance-fn)
                [2 37])
           (is= (align-positions test-input distance-2-fn)
                [5 168]))}
  [input distance-fn]
  (let [positions (->> (clojure.string/split input #",")
                       (map read-string))]
    (->> (range)
         (map (fn [r]
                (->> positions
                     (map (fn [p] (distance-fn p r)))
                     (reduce +))))
         (partition 2 1)
         (map-indexed (fn [index item] [index item]))
         (some (fn [[index [p1 p2]]] (when (< p1 p2) [index p1]))))))


(comment
  (is= (time (align-positions puzzle-input distance-fn))
       ; "Elapsed time: 1068.78478 msecs"
       [349 356992]))

(comment
  (is= (time (align-positions puzzle-input distance-2-fn))
       ; "Elapsed time: 3386.525335 msecs" without memoize
       ; "Elapsed time: 151.147587 msecs"
       [489 101268110]))

