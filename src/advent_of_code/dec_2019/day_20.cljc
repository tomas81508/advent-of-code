(ns advent-of-code.dec-2019.day-20
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.collections :refer [seq-contains?]]
            [clojure.string :refer [join]]))

(def test-maze-1 ["         A           "
                  "         A           "
                  "  #######.#########  "
                  "  #######.........#  "
                  "  #######.#######.#  "
                  "  #######.#######.#  "
                  "  #######.#######.#  "
                  "  #####  B    ###.#  "
                  "BC...##  C    ###.#  "
                  "  ##.##       ###.#  "
                  "  ##...DE  F  ###.#  "
                  "  #####    G  ###.#  "
                  "  #########.#####.#  "
                  "DE..#######...###.#  "
                  "  #.#########.###.#  "
                  "FG..#########.....#  "
                  "  ###########.#####  "
                  "             Z       "
                  "             Z       "])

(def test-maze-2 ["                   A               "
                  "                   A               "
                  "  #################.#############  "
                  "  #.#...#...................#.#.#  "
                  "  #.#.#.###.###.###.#########.#.#  "
                  "  #.#.#.......#...#.....#.#.#...#  "
                  "  #.#########.###.#####.#.#.###.#  "
                  "  #.............#.#.....#.......#  "
                  "  ###.###########.###.#####.#.#.#  "
                  "  #.....#        A   C    #.#.#.#  "
                  "  #######        S   P    #####.#  "
                  "  #.#...#                 #......VT"
                  "  #.#.#.#                 #.#####  "
                  "  #...#.#               YN....#.#  "
                  "  #.###.#                 #####.#  "
                  "DI....#.#                 #.....#  "
                  "  #####.#                 #.###.#  "
                  "ZZ......#               QG....#..AS"
                  "  ###.###                 #######  "
                  "JO..#.#.#                 #.....#  "
                  "  #.#.#.#                 ###.#.#  "
                  "  #...#..DI             BU....#..LF"
                  "  #####.#                 #.#####  "
                  "YN......#               VT..#....QG"
                  "  #.###.#                 #.###.#  "
                  "  #.#...#                 #.....#  "
                  "  ###.###    J L     J    #.#.###  "
                  "  #.....#    O F     P    #.#...#  "
                  "  #.###.#####.#.#####.#####.###.#  "
                  "  #...#.#.#...#.....#.....#.#...#  "
                  "  #.#####.###.###.#.#.#########.#  "
                  "  #...#.#.....#...#.#.#.#.....#.#  "
                  "  #.###.#####.###.###.#.#.#######  "
                  "  #.#.........#...#.............#  "
                  "  #########.###.###.#############  "
                  "           B   J   C               "
                  "           U   P   P               "])

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2019/day_20.txt")
      (clojure.string/split-lines)))


(defn find-warps
  {:test (fn []
           (is= (find-warps {:open-passage #{[3 15] [13 3] [13 15] [17 5] [2 8] [17 6] [12 13] [15 3] [5 10] [17 12] [9 6] [11 13]
                                             [9 3] [4 10] [3 13] [4 9] [2 13] [16 3] [14 15] [17 4] [11 12] [4 8] [13 13] [11 3]
                                             [2 15] [17 3] [17 14] [17 10] [9 2] [17 15] [17 11] [3 14] [12 3] [13 16] [17 13] [9 5]
                                             [6 10] [3 8] [9 4] [16 15] [14 3] [17 9] [15 15] [10 3] [13 14] [17 8] [17 7]}
                             :letters      {[9 8]   \C [1 15] \G [9 0] \A [1 13] \E [0 13] \D [8 10] \E [0 15] \F [11 11] \G
                                            [11 10] \F [1 8] \C [13 17] \Z [7 10] \D [9 1] \A [9 7] \B [13 18] \Z [0 8] \B}})
                {:goal   [13 16]
                 [2 8]   [9 6]
                 [9 6]   [2 8]
                 [2 13]  [6 10]
                 [11 12] [2 15]
                 [2 15]  [11 12]
                 :start  [9 2]
                 [6 10]  [2 13]}))}
  [{open-passage :open-passage letter-positions :letters}]
  (->> (reduce-kv (fn [a p l]
                    (let [adjacent-letter-position (->> [[-1 0] [1 0] [0 1] [0 -1]]
                                                        (map (fn [d] (mapv + d p)))
                                                        (filter letter-positions)
                                                        (first))
                          adjacent-letter (letter-positions adjacent-letter-position)
                          k (join (sort [l adjacent-letter]))
                          positions (sort [p adjacent-letter-position])]
                      (if (and (contains? a k) (seq-contains? (get a k) positions))
                        a
                        (update a k conj positions))))
                  {}
                  letter-positions)
       ; {"BC" (([0 8] [1 8]) ([9 7] [9 8])),
       ; "FG" (([11 10] [11 11]) ([0 15] [1 15])),
       ; "AA" (([9 0] [9 1])),
       ; "DE" (([7 10] [8 10]) ([0 13] [1 13])),
       ; "ZZ" (([13 17] [13 18]))}
       (reduce-kv (fn [a letters positions]
                    (let [warp-positions (->> positions
                                              (map (fn [pairs]
                                                     (->> [[-1 0] [1 0] [0 1] [0 -1]]
                                                          (map (fn [d]
                                                                 (->> pairs
                                                                      (map (fn [p] (mapv + p d))))))
                                                          (apply concat)
                                                          (filter open-passage)
                                                          (first)))))]
                      (cond (= letters "AA")
                            (assoc a :start (or (first warp-positions) (second warp-positions)))

                            (= letters "ZZ")
                            (assoc a :goal (or (first warp-positions) (second warp-positions)))

                            :else
                            (assoc a (first warp-positions) (second warp-positions)
                                     (second warp-positions) (first warp-positions)))))
                  {})
       ))

(defn create-state
  {:test (fn []
           (is= (create-state test-maze-1)
                {:open-passage #{[3 15] [13 3] [13 15] [17 5] [2 8] [17 6] [12 13] [15 3] [5 10] [17 12] [9 6] [11 13]
                                 [9 3] [4 10] [3 13] [4 9] [2 13] [16 3] [14 15] [17 4] [11 12] [4 8] [13 13] [11 3]
                                 [2 15] [17 3] [17 14] [17 10] [9 2] [17 15] [17 11] [3 14] [12 3] [13 16] [17 13] [9 5]
                                 [6 10] [3 8] [9 4] [16 15] [14 3] [17 9] [15 15] [10 3] [13 14] [17 8] [17 7]}
                 :start        [9 2]
                 :goal         [13 16]
                 :warps        {[2 8]   [9 6]
                                [9 6]   [2 8]
                                [11 12] [2 15]
                                [2 15]  [11 12]
                                [6 10]  [2 13]
                                [2 13]  [6 10]}}))}
  [strings]
  (let [{open-passage :open-passage
         letters      :letters}
        (->> strings
             (map-indexed
               (fn [y row]
                 (->> row
                      (map-indexed
                        (fn [x l]
                          {:position [x y]
                           :letter   l})))))
             (flatten)
             (reduce (fn [a {position :position l :letter}]
                       (cond (= l \space) a
                             (= l \#) a
                             (= l \.) (update a :open-passage conj position)
                             :else (assoc-in a [:letters position] l)))
                     {:open-passage #{}
                      :letters      {}}))
        warps (find-warps {:open-passage open-passage
                           :letters      letters})]
    {:open-passage open-passage
     :start        (:start warps)
     :goal         (:goal warps)
     :warps        (dissoc warps :start :goal)}))


(defn move-iteration
  {:test (fn []
           (is= (let [state (create-state test-maze-1)]
                  (move-iteration state 0 {(:start state) 0}))
                {[9 2] 0
                 [9 3] 1})
           (is= (let [state (create-state test-maze-1)]
                  (->> {(:start state) 0}
                       (move-iteration state 0)
                       (move-iteration state 1)
                       (move-iteration state 2)
                       (move-iteration state 3)
                       (move-iteration state 4)
                       (move-iteration state 5)))
                {[9 2]  0 [9 3] 1 [9 4] 2 [9 5] 3 [9 6] 4 [2 8] 5 [3 8] 6
                 [10 3] 2 [11 3] 3 [12 3] 4 [13 3] 5 [14 3] 6}))}
  [state movements visits]
  (let [current-positions (->> (seq visits)
                               (filter (fn [[_ m]] (= m movements)))
                               (map first))
        new-positions (->> current-positions
                           (map (fn [p]
                                  (let [warp-point (get (:warps state) p)]
                                    (concat (if warp-point [warp-point] [])
                                            (->> [[-1 0] [1 0] [0 -1] [0 1]]
                                                 (map (fn [d] (mapv + p d)))
                                                 (filter (partial contains? (:open-passage state))))))))
                           (apply concat))]
    (reduce (fn [visits p]
              (if (contains? visits p)
                visits
                (assoc visits p (inc movements))))
            visits
            new-positions)))

(defn move-to-goal
  {:test (fn []
           (is= (-> (create-state test-maze-1)
                    (move-to-goal))
                23)
           (is= (-> (create-state test-maze-2)
                    (move-to-goal))
                58))}
  [state]
  (loop [movements 0
         visits {(:start state) 0}]
    (let [visits (move-iteration state movements visits)]
      (if (contains? visits (:goal state))
        (inc movements)
        (recur (inc movements)
               visits)))))

(deftest puzzle-a
  (is= (time (-> (get-puzzle-input)
                 (create-state)
                 (move-to-goal)))
       ; "Elapsed time: 166.052141 msecs"
       454))
