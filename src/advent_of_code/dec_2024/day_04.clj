(ns advent-of-code.dec-2024.day-04
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is is-not]]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_04_input.txt")
                (clojure.string/split-lines)))

(def test-input ["MMMSXXMASM"
                 "MSAMXMSMSA"
                 "AMXSXMAAMM"
                 "MSAMASMSMX"
                 "XMASAMXAMM"
                 "XXAMMXXAMA"
                 "SMSMSASXSS"
                 "SAXAMASAAA"
                 "MAMMMXMMMM"
                 "MXMXAXMASX"])

(defn create-state
  [input]
  (->> input
       (reduce-kv (fn [a y row]
                    (->> (into [] (seq row))
                         (reduce-kv (fn [a x c]
                                      (assoc a [x y] c))
                                    a)))
                  {})))

(def state (create-state input))
(def test-state (create-state test-input))

(def direction (for [x (range -1 2)
                     y (range -1 2)
                     :when (not= [x y] [0 0])]
                 [x y]))

(defn vector-add [u v] (map + u v))
(defn vector-multiply [u s] (map (fn [x] (* x s)) u))

(defn number-of-xmas
  {:test (fn []
           (is= (number-of-xmas {[4 4] \X [3 3] \M [2 2] \A [1 1] \S
                                 [4 3] \M [4 2] \A [4 1] \S
                                 [4 5] \M [4 6] \A}
                                [4 4])
                2))}
  [state position]
  (if (not= (get state position) \X)
    0
    (->> direction
         (filter (fn [d]
                   (and (= (get state (vector-add position d)) \M)
                        (= (get state (vector-add position (vector-multiply d 2))) \A)
                        (= (get state (vector-add position (vector-multiply d 3))) \S))))
         (count))))

(comment
  (->> (keys test-state)
       (map (fn [p] (number-of-xmas test-state p)))
       (reduce +))

  (->> (keys state)
       (map (fn [p] (number-of-xmas state p)))
       (reduce +))

  )

(def test-input-2 [".M.S......"
                   "..A..MSMS."
                   ".M.S.MAA.."
                   "..A.ASMSM."
                   ".M.S.M...."
                   ".........."
                   "S.S.S.S.S."
                   ".A.A.A.A.."
                   "M.M.M.M.M."
                   ".........."])

(def test-state-2 (create-state test-input-2))

(def diagonal-directions [[-1 -1] [-1 1] [1 1] [1 -1]])

(defn number-of-x-mas
  {:test (fn []
           (is= (number-of-x-mas (create-state ["M.S"
                                                ".A."
                                                "M.S"])
                                 [1 1])
                1)
           (is= (number-of-x-mas (create-state ["S.."
                                                ".A."
                                                "..M"])
                                 [1 1])
                0))}
  [state position]
  (if (not= (get state position) \A)
    0
    (if (= 2 (->> diagonal-directions
                  (filter (fn [d]
                            (and (= (get state (vector-add position d)) \M)
                                 (= (get state (vector-add position (vector-multiply d -1))) \S))))
                  (count)))
      1
      0)))

(comment
  (->> (keys test-state)
       (map (fn [p] (number-of-x-mas test-state p)))
       (reduce +))

  (->> (keys test-state-2)
       (map (fn [p] (number-of-x-mas test-state-2 p)))
       (reduce +))

  (->> (keys state)
       (map (fn [p] (number-of-x-mas state p)))
       (reduce +))

  )






