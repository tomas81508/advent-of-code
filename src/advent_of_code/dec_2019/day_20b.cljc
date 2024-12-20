(ns advent-of-code.dec-2019.day-20b
  (:require [advent-of-code.test :refer [is=]]
            [advent-of-code.collections :refer [seq-contains?]]
            [clojure.string :refer [join]]
            [advent-of-code.dec-2019.day-20 :refer [test-maze-1
                                                    get-puzzle-input]]))


(def test-maze-3 ["             Z L X W       C                 "
                  "             Z P Q B       K                 "
                  "  ###########.#.#.#.#######.###############  "
                  "  #...#.......#.#.......#.#.......#.#.#...#  "
                  "  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  "
                  "  #.#...#.#.#...#.#.#...#...#...#.#.......#  "
                  "  #.###.#######.###.###.#.###.###.#.#######  "
                  "  #...#.......#.#...#...#.............#...#  "
                  "  #.#########.#######.#.#######.#######.###  "
                  "  #...#.#    F       R I       Z    #.#.#.#  "
                  "  #.###.#    D       E C       H    #.#.#.#  "
                  "  #.#...#                           #...#.#  "
                  "  #.###.#                           #.###.#  "
                  "  #.#....OA                       WB..#.#..ZH"
                  "  #.###.#                           #.#.#.#  "
                  "CJ......#                           #.....#  "
                  "  #######                           #######  "
                  "  #.#....CK                         #......IC"
                  "  #.###.#                           #.###.#  "
                  "  #.....#                           #...#.#  "
                  "  ###.###                           #.#.#.#  "
                  "XF....#.#                         RF..#.#.#  "
                  "  #####.#                           #######  "
                  "  #......CJ                       NM..#...#  "
                  "  ###.#.#                           #.###.#  "
                  "RE....#.#                           #......RF"
                  "  ###.###        X   X       L      #.#.#.#  "
                  "  #.....#        F   Q       P      #.#.#.#  "
                  "  ###.###########.###.#######.#########.###  "
                  "  #.....#...#.....#.......#...#.....#.#...#  "
                  "  #####.#.###.#######.#######.###.###.#.#.#  "
                  "  #.......#.......#.#.#.#.#...#...#...#.#.#  "
                  "  #####.###.#####.#.#.#.#.###.###.#.###.###  "
                  "  #.......#.....#.#...#...............#...#  "
                  "  #############.#.#.###.###################  "
                  "               A O F   N                     "
                  "               A A D   M                     "])

(defn find-warps
  {:test (fn []
           (is= (find-warps {:open-passage #{[3 15] [13 3] [13 15] [17 5] [2 8] [17 6] [12 13] [15 3] [5 10] [17 12] [9 6] [11 13]
                                             [9 3] [4 10] [3 13] [4 9] [2 13] [16 3] [14 15] [17 4] [11 12] [4 8] [13 13] [11 3]
                                             [2 15] [17 3] [17 14] [17 10] [9 2] [17 15] [17 11] [3 14] [12 3] [13 16] [17 13] [9 5]
                                             [6 10] [3 8] [9 4] [16 15] [14 3] [17 9] [15 15] [10 3] [13 14] [17 8] [17 7]}
                             :letters      {[9 8]   \C [1 15] \G [9 0] \A [1 13] \E [0 13] \D [8 10] \E [0 15] \F [11 11] \G
                                            [11 10] \F [1 8] \C [13 17] \Z [7 10] \D [9 1] \A [9 7] \B [13 18] \Z [0 8] \B}})
                {:inner-warps {[9 6]   [2 8]
                               [11 12] [2 15]
                               [6 10]  [2 13]}
                 :outer-warps {[2 8]  [9 6]
                               [2 15] [11 12]
                               [2 13] [6 10]}
                 :start       [9 2]
                 :goal        [13 16]}))}
  [{open-passage :open-passage letter-positions :letters}]
  (let [{max-x :max-x max-y :max-y} (->> open-passage
                                         (reduce (fn [{max-x :max-x max-y :max-y} [x y]]
                                                   {:max-x (max max-x x)
                                                    :max-y (max max-y y)})
                                                 {:max-x 0 :max-y 0}))]
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
                        (let [[x1 y1 :as wp1] (first warp-positions)
                              [x2 y2 :as wp2] (second warp-positions)]
                          (cond (= letters "AA")
                                (assoc a :start (or wp1 wp2))

                                (= letters "ZZ")
                                (assoc a :goal (or wp1 wp2))

                                (or (= x1 2) (= x1 max-x) (= y1 2) (= y1 max-y))
                                (-> a
                                    (assoc-in [:outer-warps wp1] wp2)
                                    (assoc-in [:inner-warps wp2] wp1))

                                :else
                                (-> a
                                    (assoc-in [:outer-warps wp2] wp1)
                                    (assoc-in [:inner-warps wp1] wp2))))))
                    {:inner-warps {}
                     :outer-warps {}}))))

(defn create-state
  {:test (fn []
           (is= (create-state test-maze-1)
                {:open-passage #{[3 15] [13 3] [13 15] [17 5] [2 8] [17 6] [12 13] [15 3] [5 10] [17 12] [9 6] [11 13]
                                 [9 3] [4 10] [3 13] [4 9] [2 13] [16 3] [14 15] [17 4] [11 12] [4 8] [13 13] [11 3]
                                 [2 15] [17 3] [17 14] [17 10] [9 2] [17 15] [17 11] [3 14] [12 3] [13 16] [17 13] [9 5]
                                 [6 10] [3 8] [9 4] [16 15] [14 3] [17 9] [15 15] [10 3] [13 14] [17 8] [17 7]}
                 :level        0
                 :start        [9 2]
                 :goal         [13 16]
                 :inner-warps  {[9 6]   [2 8]
                                [11 12] [2 15]
                                [6 10]  [2 13]}
                 :outer-warps  {[2 8]  [9 6]
                                [2 15] [11 12]
                                [2 13] [6 10]}}))}
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
     :level        0
     :start        (:start warps)
     :goal         (:goal warps)
     :inner-warps  (:inner-warps warps)
     :outer-warps  (:outer-warps warps)}))


(defn move-iteration
  {:test (fn []
           (is= (let [state (create-state test-maze-1)]
                  (move-iteration state 0 {{:position (:start state) :level 0} 0}))
                {{:position [9 2] :level 0} 0
                 {:position [9 3] :level 0} 1})
           (is= (let [state (create-state test-maze-1)]
                  (->> {{:position (:start state) :level 0} 0}
                       (move-iteration state 0)
                       (move-iteration state 1)
                       (move-iteration state 2)
                       (move-iteration state 3)
                       (move-iteration state 4)
                       (move-iteration state 5)))
                {{:position [9 2] :level 0}  0
                 {:position [9 3] :level 0}  1
                 {:position [9 4] :level 0}  2
                 {:position [9 5] :level 0}  3
                 {:position [9 6] :level 0}  4
                 {:position [2 8] :level 1}  5
                 {:position [3 8] :level 1}  6
                 {:position [10 3] :level 0} 2
                 {:position [11 3] :level 0} 3
                 {:position [12 3] :level 0} 4
                 {:position [13 3] :level 0} 5
                 {:position [14 3] :level 0} 6}))}
  [state movements visits]
  (let [current-positions (->> (seq visits)
                               (filter (fn [[_ m]] (= m movements)))
                               (map first))
        new-positions (->> current-positions
                           (map (fn [{p :position l :level}]
                                  (let [inner-warp-point (get (:inner-warps state) p)
                                        outer-warp-point (when (pos? l) (get (:outer-warps state) p))]
                                    (concat (cond-> []
                                                    inner-warp-point (conj {:position inner-warp-point :level (inc l)})
                                                    outer-warp-point (conj {:position outer-warp-point :level (dec l)}))
                                            (->> [[-1 0] [1 0] [0 -1] [0 1]]
                                                 (map (fn [d] (mapv + p d)))
                                                 (filter (partial contains? (:open-passage state)))
                                                 (map (fn [p] {:position p :level l})))))))
                           (apply concat))]
    (reduce (fn [visits p]
              (if (contains? visits p)
                visits
                (assoc visits p (inc movements))))
            visits
            new-positions)))

(defn move-to-goal
  {:test (fn []
           (is= (-> (create-state test-maze-3)
                    (move-to-goal))
                396))}
  [state]
  (loop [movements 0
         visits {{:position (:start state) :level 0} 0}]
    (let [visits (move-iteration state movements visits)]
      (if (contains? visits {:position (:goal state) :level 0})
        (inc movements)
        (recur (inc movements)
               visits)))))

(deftest puzzle-b
         (is= (time (-> (get-puzzle-input)
                        (create-state)
                        (move-to-goal)))
              ;"Elapsed time: 131260.384125 msecs"
              5744))