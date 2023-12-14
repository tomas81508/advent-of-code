(ns advent-of-code.dec-2023.day-13
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]))

(def input (as-> (slurp "src/advent_of_code/dec_2023/day_13_input.txt") $
                 (clojure.string/split $ #"\n\n")
                 (map (fn [s] (clojure.string/split-lines s)) $)))

(def test-input-1 ["#.##..##."
                   "..#.##.#."
                   "##......#"
                   "##......#"
                   "..#.##.#."
                   "..##..##."
                   "#.#.##.#."])

(def test-input-2 ["#...##..#"
                   "#....#..#"
                   "..##..###"
                   "#####.##."
                   "#####.##."
                   "..##..###"
                   "#....#..#"])

(defn transpose-input
  {:test (fn []
           (is= (transpose-input ["ab" "cd"]) [[\a \c] [\b \d]]))}
  [lines]
  (->> lines
       (map-indexed (fn [y line] [y line]))
       (reduce (fn [a [y line]]
                 (->> line
                      (map-indexed (fn [x c] [x c]))
                      (reduce (fn [a [x c]]
                                (assoc-in a [x y] c))
                              a)))
               (into [] (repeat (count (first lines)) [])))))

(defn find-column-reflection
  {:test (fn []
           (is= (find-column-reflection test-input-2) 4))}
  [lines]
  (loop [current 1]
    (let [first-half (reverse (take current lines))
          second-half (drop current lines)]
      (cond (empty? second-half)
            nil

            (->> (map = first-half second-half)
                 (every? true?))
            current

            :else
            (recur (inc current))))))

(defn find-reflection-score
  {:test (fn []
           (is= (find-reflection-score test-input-1) 5)
           (is= (find-reflection-score test-input-2) 400))}
  [lines]
  (or (when-let [column (find-column-reflection lines)]
        (* 100 column))
      (find-column-reflection (transpose-input lines))))

(deftest puzzle-a
  (is= (time (->> input
                  (map find-reflection-score)
                  (reduce +)))
       ; "Elapsed time: 10.385163 msecs"
       31877))

; part 2

(defn find-column-reflection-2
  {:test (fn []
           (is= (find-column-reflection-2 test-input-1) 3)
           (is= (find-column-reflection-2 test-input-2) 1))}
  [lines]
  (loop [current 1]
    (let [first-half (reverse (take current lines))
          second-half (drop current lines)]
      (cond (empty? second-half)
            nil

            (= 1 (->> (map (fn [l1 l2]
                             (->> (map = l1 l2)
                                  (remove true?)
                                  (count)))
                           first-half
                           second-half)
                      (reduce +)))
            current

            :else
            (recur (inc current))))))

(defn find-reflection-score-2
  {:test (fn []
           (is= (find-reflection-score-2 test-input-1) 300)
           (is= (find-reflection-score-2 test-input-2) 100))}
  [lines]
  (or (when-let [column (find-column-reflection-2 lines)]
        (* 100 column))
      (find-column-reflection-2 (transpose-input lines))))

(deftest puzzle-b
  (is= (time (->> input
                  (map find-reflection-score-2)
                  (reduce +)))
       ; "Elapsed time: 24.386291 msecs"
       42996))


