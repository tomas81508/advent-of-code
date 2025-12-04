(ns advent-of-code.dec-2025.day-04
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

; together with Daniel Gullberg

(def input (->> (slurp "src/advent_of_code/dec_2025/day_04_input.txt")
                (string/split-lines)
                (map (comp vec seq))
                (vec)))

(def test-input (->> ["..@@.@@@@."
                      "@@@.@.@.@@"
                      "@@@@@.@.@@"
                      "@.@@@@..@."
                      "@@.@@@@.@@"
                      ".@@@@@@@.@"
                      ".@.@.@.@@@"
                      "@.@@@.@@@@"
                      ".@@@@@@@@."
                      "@.@.@@@.@."]
                     (map (comp vec seq))
                     (vec)))

(defn paper?
  {:test (fn []
           (is (paper? test-input [2 0]))
           (is-not (paper? test-input [0 0])))}
  [input [x y]]
  (= (get-in input [y x]) \@))

(def directions (for [x (range -1 2)
                      y (range -1 2)
                      :when (not= [x y] [0 0])]
                  [x y]))

(defn count-adjacent-paper
  {:test (fn []
           (is= (count-adjacent-paper test-input [0 0]) 2))}
  [input position]
  (->> directions
       (map (fn [d] (map + d position)))
       (filter (fn [p] (paper? input p)))
       (count)))

(defn get-rolls-of-paper
  {:test (fn []
           (is= (count (get-rolls-of-paper test-input))
                13))}
  [input]
  (->> (for [y (range (count input))
             x (range (count (first input)))
             :when (paper? input [x y])]
         [x y])
       (filter (fn [p] (< (count-adjacent-paper input p) 4)))))

(comment
  (count (get-rolls-of-paper input))
  )

; part 2
(defn part-2
  {:test (fn []
           (is= (part-2 test-input)
                43))}
  [input]
  (loop [input input
         removed 0]
    (let [rolls-of-paper (get-rolls-of-paper input)]
      (if (empty? rolls-of-paper)
        removed
        (recur (reduce (fn [a p]
                         (assoc-in a (reverse p) \x))
                       input
                       rolls-of-paper)
               (+ removed (count rolls-of-paper)))))))

(comment
  (part-2 input)
  )