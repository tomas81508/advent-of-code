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
                             :cubes      #{[0 0]}}
                            2
                            2
                            [0 -1])
                {:stable     #{[0 1]}
                 :non-stable #{}
                 :cubes      #{[0 0]}})
           (is= (move-rocks {:stable     #{[0 0]}
                             :non-stable #{[0 1]}
                             :cubes      #{}}
                            2
                            2
                            [0 -1])
                {:stable     #{[0 0] [0 1]}
                 :non-stable #{}
                 :cubes      #{}})
           (is= (move-rocks {:stable     #{}
                             :non-stable #{[0 2]}
                             :cubes      #{}}
                            2
                            2
                            [0 -1])
                {:stable     #{[0 0]}
                 :non-stable #{}
                 :cubes      #{}})
           (is= (move-rocks {:stable     #{}
                             :non-stable #{[0 2] [0 1]}
                             :cubes      #{}}
                            2
                            2
                            [0 -1])
                {:stable     #{[0 0] [0 1]}
                 :non-stable #{}
                 :cubes      #{}})
           (is= (-> ["#" "O" "." "O"]
                    (create-state)
                    (move-rocks 4 4 [0 -1])
                    (:stable))
                #{[0 1] [0 2]}))}
  [state max-x max-y direction]
  (reduce (fn [state rock]
            (-> (loop [coordinate rock
                       best-coordinate rock]
                  (let [next-coordinate (map + coordinate direction)]
                    (cond (or (cube? state next-coordinate)
                              (stable-rock? state next-coordinate)
                              (not (<= 0 (first next-coordinate) (dec max-x)))
                              (not (<= 0 (second next-coordinate) (dec max-y))))
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
                   max-x (get-in state [:size :x])
                   max-y (get-in state [:size :y])]
               (->> (move-rocks state max-x max-y [0 -1])
                    (:stable)
                    (map (fn [[_ y]] (- max-y y)))
                    (reduce +))))
       136))


(deftest puzzle-a
  (is= (time (let [state (create-state input)
                   max-x (get-in state [:size :x])
                   max-y (get-in state [:size :y])]
               (->> (move-rocks state max-x max-y [0 -1])
                    (:stable)
                    (map (fn [[_ y]] (- max-y y)))
                    (reduce +))))
       ; "Elapsed time: 16.928976 msecs"
       108792))

;; part 2

(deftest puzzle-b
  (is= (time (let [state (create-state input)
                   max-x (get-in state [:size :x])
                   max-y (get-in state [:size :y])]
               (loop [state state
                      i 0
                      configurations {(hash state) i}]
                 (let [state (->> [[0 -1] [-1 0] [0 1] [1 0]]
                                  (reduce (fn [state d]
                                            (let [result (move-rocks state max-x max-y d)]
                                              (-> state
                                                  (assoc :stable #{})
                                                  (assoc :non-stable (:stable result)))))
                                          state))
                       state-hash (hash state)]
                   (if (not (contains? configurations state-hash))
                     (recur state (+ i 4) (assoc configurations state-hash i))
                     (let [previous-i (get configurations state-hash)
                           loop-size (- i previous-i)
                           r (rem (- 1000000000 previous-i) loop-size)]
                       (->> (cycle [[0 -1] [-1 0] [0 1] [1 0]])
                            (take r)
                            (reduce (fn [state d]
                                      (let [result (move-rocks state max-x max-y d)]
                                        (-> state
                                            (assoc :stable #{})
                                            (assoc :non-stable (:stable result)))))
                                    state)
                            (:non-stable)
                            (map (fn [[_ y]] (- max-y y)))
                            (reduce +))))))))
       ; "Elapsed time: 6775.730689 msecs"
       99118)
  )




















