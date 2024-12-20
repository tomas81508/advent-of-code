(ns advent-of-code.dec-2018.day-03
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2018/day_03.txt")
       (clojure.string/split-lines)))

(defn parse-claim
  {:test (fn []
           (is= (parse-claim "#123 @ 3,2: 5x4")
                {:id     "123"
                 :x      3
                 :y      2
                 :width  5
                 :height 4}))}
  [claim]
  (let [[_ id x y width height] (re-find (re-pattern "#([\\d]+) @ ([\\d]+),([\\d]+): ([\\d]+)x([\\d]+)") claim)]
    {:id id :x (read-string x) :y (read-string y) :width (read-string width) :height (read-string height)}))

(defn get-coordinates
  [{x :x y :y width :width height :height}]
  (for [x (range x (+ x width))
        y (range y (+ y height))]
    [x y]))


(defn get-fabric
  {:test (fn []
           (is= (get-fabric ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
                {[3 1] 1 [4 1] 1 [5 1] 1 [6 1] 1
                 [3 2] 1 [4 2] 1 [5 2] 1 [6 2] 1
                 [1 3] 1 [2 3] 1 [3 3] 2 [4 3] 2 [5 3] 1 [6 3] 1
                 [1 4] 1 [2 4] 1 [3 4] 2 [4 4] 2 [5 4] 1 [6 4] 1
                 [1 5] 1 [2 5] 1 [3 5] 1 [4 5] 1 [5 5] 1 [6 5] 1
                 [1 6] 1 [2 6] 1 [3 6] 1 [4 6] 1 [5 6] 1 [6 6] 1}))}
  [claims]
  (->> claims
       (map parse-claim)
       (reduce (fn [a v]
                 (->> (get-coordinates v)
                      (reduce (fn [a v]
                                (if (contains? a v)
                                  (update a v inc)
                                  (assoc a v 1)))
                              a)))
               {})))

(deftest puzzle-part-1
  (is= (->> (get-puzzle-input)
            (get-fabric)
            (vals)
            (filter (fn [x] (> x 1)))
            (count))
       96569))


(defn overlap?
  {:test (fn []
           (let [fabric {[3 1] 1 [4 1] 1 [5 1] 1 [6 1] 1
                         [3 2] 1 [4 2] 1 [5 2] 1 [6 2] 1
                         [1 3] 1 [2 3] 1 [3 3] 2 [4 3] 2 [5 3] 1 [6 3] 1
                         [1 4] 1 [2 4] 1 [3 4] 2 [4 4] 2 [5 4] 1 [6 4] 1
                         [1 5] 1 [2 5] 1 [3 5] 1 [4 5] 1 [5 5] 1 [6 5] 1
                         [1 6] 1 [2 6] 1 [3 6] 1 [4 6] 1 [5 6] 1 [6 6] 1}]
             (is (overlap? fabric "#1 @ 1,3: 4x4"))
             (is (overlap? fabric "#2 @ 3,1: 4x4"))
             (is-not (overlap? fabric "#3 @ 5,5: 2x2"))))}
  [fabric claim]
  (->> (parse-claim claim)
       (get-coordinates)
       (remove (fn [c] (= (get fabric c) 1)))
       (empty?)
       (not)))


(deftest puzzle-part-2
  (time (is= (let [fabric (->> (get-puzzle-input)
                               (get-fabric))]
               (->> (get-puzzle-input)
                    (filter (fn [c] (not (overlap? fabric c))))
                    (first)
                    (parse-claim)
                    (:id)))
             "1023")))