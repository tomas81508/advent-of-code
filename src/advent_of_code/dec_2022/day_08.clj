(ns advent-of-code.dec-2022.day-08
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def test-input ["30373"
                 "25512"
                 "65332"
                 "33549"
                 "35390"])

(def input (->> (slurp "src/advent_of_code/dec_2022/day_08_input.txt")
                (clojure.string/split-lines)))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn get-tree-height
  {:test (fn []
           (is= (get-tree-height test-input [1 1])
                5))}
  [forest [x y]]
  (read-string (str (get-in forest [y x]))))

(defn in-the-forest?
  {:test (fn []
           (is (in-the-forest? test-input [1 1]))
           (is (in-the-forest? test-input [4 1]))
           (is-not (in-the-forest? test-input [-1 1]))
           (is-not (in-the-forest? test-input [6 1])))}
  [forest [x y]]
  (get-in forest [y x]))

(defn visible?
  {:test (fn []
           (is (visible? test-input [0 0]))
           (is (visible? test-input [1 1]))
           (is-not (visible? test-input [3 1])))}
  [forest position]
  (let [tree-height (get-tree-height forest position)]
    (->> directions
         (some (fn [d]
                 (loop [current (map + d position)]
                   (cond (not (in-the-forest? forest current))
                         true

                         (>= (get-tree-height forest current) tree-height)
                         false

                         :else
                         (recur (map + d current)))))))))

(defn count-visible-trees
  {:test (fn []
           (is= (count-visible-trees test-input)
                21))}
  [forest]
  (->> (for [y (range (count forest))
             x (range (count (first forest)))]
         [x y])
       (filter (fn [position]
                 (visible? forest position)))
       (count)))

(comment
  (time (count-visible-trees input))
  ; "Elapsed time: 160.484618 msecs"
  ; => 1809
  )

(defn tree-view
  {:test (fn []
           (is= (tree-view test-input [2 1])
                [1 1 2 2]))}
  [forest position]
  (let [tree-height (get-tree-height forest position)]
    (->> directions
         (map (fn [d]
                (loop [count 0
                       current (map + position d)]
                  (cond (not (in-the-forest? forest current))
                        count

                        (>= (get-tree-height forest current) tree-height)
                        (inc count)

                        :else
                        (recur (inc count) (map + current d)))))))))

(defn solve-puzzle-b
  [forest]
  (->> (for [y (range (count forest))
             x (range (count (first forest)))]
         [x y])
       (map (fn [position] (apply * (tree-view forest position))))
       (apply max)))

(comment
  (time (solve-puzzle-b input))
  ; "Elapsed time: 180.646942 msecs"
  ; => 479400
  )

