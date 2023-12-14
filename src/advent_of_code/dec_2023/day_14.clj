(ns advent-of-code.dec-2023.day-14
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]))


(def input (->> (slurp "src/advent_of_code/dec_2023/day_14_input.txt")
                (split-lines)))

(def test-input ["O....#...."
                 "O.OO#....#"
                 ".....##..."
                 "OO.#O....O"
                 ".O.....O#."
                 "O.#..O.#.#"
                 "..O..#O..O"
                 ".......O.."
                 "#....###.."
                 "#OO..#...."])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:stable     #{}
                 :non-stable #{[4 3] [0 0] [7 7] [7 4] [0 5] [6 6] [9 6] [1 9] [9 3] [2 9] [1 4] [1 3] [0 3] [5 5] [3 1] [2 1] [2 6] [0 1]},
                 :cubes      #{[8 4] [2 5] [3 3] [7 8] [0 9] [4 1] [5 2] [5 6] [5 8] [6 8] [5 9] [9 1] [9 5] [7 5] [5 0] [6 2] [0 8]}
                 :size       {:x 10, :y 10}}))}
  [input]
  (merge (reduce (fn [a [y row]]
                   (reduce (fn [a [x c]]
                             (case c
                               \. a
                               \O (update a :non-stable conj [x y])
                               \# (update a :cubes conj [x y])))
                           a
                           (map-indexed (fn [x c] [x c]) row)))
                 {:stable     #{}
                  :non-stable #{}
                  :cubes      #{}}
                 (map-indexed (fn [y row] [y row]) input))
         {:size {:x (count (first input))
                 :y (count input)}}))

(defn cube? [state coordinate] (contains? (:cubes state) coordinate))
(defn stable-rock? [state coordinate] (contains? (:stable state) coordinate))
(defn non-stable-rock? [state coordinate] (contains? (:non-stable state) coordinate))

(defn move-rocks
  {:test (fn []
           (is= (move-rocks {:stable     #{}
                             :non-stable #{[0 1]}
                             :cubes      #{[0 0]}})
                {:stable     #{[0 1]}
                 :non-stable #{}
                 :cubes      #{[0 0]}})
           (is= (move-rocks {:stable     #{[0 0]}
                             :non-stable #{[0 1]}
                             :cubes      #{}})
                {:stable     #{[0 0] [0 1]}
                 :non-stable #{}
                 :cubes      #{}})
           (is= (move-rocks {:stable     #{}
                             :non-stable #{[0 2]}
                             :cubes      #{}})
                {:stable     #{[0 0]}
                 :non-stable #{}
                 :cubes      #{}})
           (is= (move-rocks {:stable     #{}
                             :non-stable #{[0 2] [0 1]}
                             :cubes      #{}})
                {:stable     #{[0 0] [0 1]}
                 :non-stable #{}
                 :cubes      #{}})
           (is= (->> ["#" "O" "." "O"]
                     (create-state)
                     (move-rocks)
                     (:stable))
                #{[0 1] [0 2]}))}
  [state]
  (reduce (fn [state rock]
            (-> (loop [coordinate rock
                       best-coordinate rock]
                  (let [next-coordinate (map + coordinate [0 -1])]
                    (cond (or (cube? state next-coordinate)
                              (stable-rock? state next-coordinate)
                              (neg? (second next-coordinate)))
                          (update state :stable conj best-coordinate)

                          (non-stable-rock? state next-coordinate)
                          (recur next-coordinate best-coordinate)

                          :else
                          (recur next-coordinate next-coordinate))))
                (update :non-stable disj rock)))
          state
          (:non-stable state)))


(deftest test-puzzle
  (is= (time (let [state (create-state test-input)
                   y-max (get-in state [:size :y])]
               (->> state
                    (move-rocks)
                    (:stable)
                    (map (fn [[_ y]] (- y-max y)))
                    (reduce +))))
       136))


(deftest puzzle-a
  (is= (time (let [state (create-state input)
                   y-max (get-in state [:size :y])]
               (->> state
                    (move-rocks)
                    (:stable)
                    (map (fn [[_ y]] (- y-max y)))
                    (reduce +))))
       ; "Elapsed time: 16.928976 msecs"
       108792))










