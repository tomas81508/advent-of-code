(ns advent-of-code.dec-2023.day-12
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is is= is-not]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]
            [clojure.math.combinatorics :refer [permutations]]))


(def input (->> (slurp "src/advent_of_code/dec_2023/day_12_input.txt")
                (split-lines)))

(defn parse-info
  {:test (fn []
           (is= (parse-info "???.### 1,1,3")
                ["???.###" [1 1 3]]))}
  [s]
  (let [[springs ns] (split s #" ")]
    [springs (->> (split ns #",")
                  (map read-string))]))

(defn matching-arrangement?
  {:test (fn []
           (is (matching-arrangement? "#.#.###" [1 1 3]))
           (is-not (matching-arrangement? "##..###" [1 1 3])))}
  [arrangement ns]
  (= (->> (re-seq #"\#+" arrangement)
          (map count))
     ns))

(defn create-arrangement
  {:test (fn []
           (is= (create-arrangement "???.###" [\# \# \.])
                "##..###"))}
  [pattern configuration]
  (reduce (fn [a c]
            (clojure.string/replace-first a "?" (str c)))
          pattern
          configuration))

(defn arrangements
  {:test (fn []
           (is= (arrangements "???.### 1,1,3")
                ["#.#.###"])
           (is= (arrangements ".??..??...?##. 1,1,3")
                [".#...#....###."
                 ".#....#...###."
                 "..#..#....###."
                 "..#...#...###."]))}
  [s]
  (let [[pattern clusters] (parse-info s)
        damaged-springs (reduce + clusters)
        unknown-damaged-springs (- damaged-springs
                                   (count (filter (fn [c] (= c \#)) pattern)))
        unknown-springs (count (filter (fn [c] (= c \?)) pattern))
        configurations (permutations (concat (repeat unknown-damaged-springs \#)
                                             (repeat (- unknown-springs unknown-damaged-springs) \.)))]
    (->> configurations
         (map (fn [c] (create-arrangement pattern c)))
         (filter (fn [a] (matching-arrangement? a clusters))))))

(defn count-arrangements [s] (count (arrangements s)))

(deftest puzzle-a
  (is= (time (->> input
                  (map count-arrangements)
                  (reduce +)))
       ; "Elapsed time: 3356.775801 msecs"
       7939)
  )