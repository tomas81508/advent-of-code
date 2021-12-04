(ns advent-of-code.dec-2021.day-04
  (:require [ysera.test :refer [is is-not is= deftest]]
            [ysera.collections :refer [seq-contains?]]))

(def test-input "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")

(comment
  ;; state could be something like this
  {:numbers [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]
   :boards  []})

(defn create-board
  {:test (fn []
           (is= (create-board "22 13 17 11  0\n  8  2 23  4 24\n  21  9 14 16  7\n  6 10  3 18  5\n  1 12 20 15 19")
                {[0 0] 22 [1 0] 13 [2 0] 17 [3 0] 11 [4 0] 0
                 [0 1] 8 [1 1] 2 [2 1] 23 [3 1] 4 [4 1] 24
                 [0 2] 21 [1 2] 9 [2 2] 14 [3 2] 16 [4 2] 7
                 [0 3] 6 [1 3] 10 [2 3] 3 [3 3] 18 [4 3] 5
                 [0 4] 1 [1 4] 12 [2 4] 20 [3 4] 15 [4 4] 19}))}
  [input]
  (->> (clojure.string/split input #"\n[ ]*")
       (map-indexed (fn [y line] [y (clojure.string/split line #"[ ]+")]))
       (reduce (fn [board [y ns]]
                 (->> (map-indexed (fn [x n] [x n]) ns)
                      (reduce (fn [board [x n]]
                                (assoc board [x y] (read-string n)))
                              board)))
               {})))

(def test-board (create-board "22 13 17 11  0\n  8  2 23  4 24\n  21  9 14 16  7\n  6 10  3 18  5\n  1 12 20 15 19"))

(defn create-state
  [input]
  (let [[numbers-input & boards-input] (clojure.string/split input #"\n\n[ ]*")]
    {:numbers (->> (clojure.string/split numbers-input #",")
                   (map read-string))
     :boards  (map (fn [board-input]
                     (create-board board-input))
                   boards-input)}))

(def test-state (create-state test-input))

(defn bingo?
  {:test (fn []
           (is-not (bingo? [] test-board))
           ; second row bingo
           (is (bingo? [8 2 23 4 24] test-board))
           ; second column bingo
           (is (bingo? [13 2 9 10 12] test-board)))}
  [numbers board]
  (or
    ; checking rows
    (some (fn [y] (every? (fn [x] (seq-contains? numbers (get board [x y])))
                          (range 5)))
          (range 5))
    ; checking columns
    (some (fn [x] (every? (fn [y] (seq-contains? numbers (get board [x y])))
                          (range 5)))
          (range 5))))

(defn perform-bingo
  {:test (fn []
           (is= (perform-bingo test-state)
                [12 2]))}
  [state]
  (->> (range (count (:numbers state)))
       (some (fn [numbers-index]
               (let [numbers (->> (:numbers state)
                                  (take numbers-index))]
                 (->> (:boards state)
                      (map-indexed (fn [board-index board] [board-index board]))
                      (some (fn [[board-index board]]
                              (when (bingo? numbers board)
                                [numbers-index board-index])))))))))

(defn solver
  [board actual-numbers]
  (let [reversed-board (clojure.set/map-invert board)]
    (->> (apply dissoc reversed-board actual-numbers)
         (keys)
         (apply +)
         (* (last actual-numbers)))))

(defn solver-a
  [input]
  (let [state (create-state input)
        [numbers-index board-index] (perform-bingo state)
        actual-numbers (take numbers-index (:numbers state))
        winning-board (nth (:boards state) board-index)]
    (solver winning-board actual-numbers)))

(deftest test-input-test-a
         (is= (solver-a test-input)
              4512))

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_04_input.txt"))

(comment

  (is= (time (solver-a puzzle-input))
       ; "Elapsed time: 38.93218 msecs"
       38913))

(defn perform-loosing-bingo
  {:test (fn []
           (is= (perform-loosing-bingo test-state)
                [15 (second (:boards (create-state test-input)))]))}
  [{numbers :numbers boards :boards}]
  (loop [index 1
         boards boards]
    (if (and (= (count boards) 1)
             (bingo? (take index numbers) (first boards)))
      [index (first boards)]
      (recur (inc index)
             (->> boards
                  (remove (fn [board]
                            (bingo? (take index numbers) board))))))))

(defn solver-b
  [input]
  (let [state (create-state input)
        [numbers-index loosing-board] (perform-loosing-bingo state)
        actual-numbers (take numbers-index (:numbers state))]
    (solver loosing-board actual-numbers)))

(deftest test-input-test-b
         (is= (solver-b test-input)
              1924))

(comment
  (is= (time (solver-b puzzle-input))
       ; "Elapsed time: 198.099089 msecs"
       16836))