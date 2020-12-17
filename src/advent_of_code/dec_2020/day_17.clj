(ns advent-of-code.dec-2020.day-17
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2020/day_17.txt")
      (clojure.string/split-lines)))

(def test-input [".#."
                 "..#"
                 "###"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input 3)
                {:active-cubes #{[1 0 0]
                                 [2 1 0]
                                 [0 2 0] [1 2 0] [2 2 0]}})
           (is= (create-state test-input 4)
                {:active-cubes #{[1 0 0 0]
                                 [2 1 0 0]
                                 [0 2 0 0] [1 2 0 0] [2 2 0 0]}}))}
  [strings dimensions]
  {:active-cubes (->> strings
                      (map-indexed
                        (fn [y rows]
                          (map-indexed (fn [x character]
                                         (when (= character \#)
                                           (into [] (take dimensions (concat [x y] [0 0])))))
                                       rows)))
                      (apply concat)
                      (remove nil?)
                      (set))})

(defn abs
  {:test (fn []
           (is= (abs 2) 2)
           (is= (abs 0) 0)
           (is= (abs -4) 4))}
  [x]
  (if (pos? x) x (- x)))


(defn distance
  {:test (fn []
           (is= (distance [1 2 3] [2 2 2]) 1)
           (is= (distance [1 2 3] [0 2 3]) 1)
           (is= (distance [1 2 3 4] [2 2 3 3]) 1)
           (is= (distance [1 2 3 4] [0 2 3 4]) 1))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))


(defn neighbors
  {:test (fn []
           (is (neighbors [1 2 3] [2 2 2]))
           (is (neighbors [1 2 3] [0 2 3]))
           (is-not (neighbors [0 0 0] [2 1 1]))
           (is (neighbors [1 2 3 4] [2 2 2 4]))
           (is (neighbors [1 2 3 4] [0 2 3 4]))
           (is-not (neighbors [0 0 0 0] [2 1 1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

(defn active?
  {:test (fn []
           (is (-> (create-state ["# "] 3)
                   (active? [0 0 0])))
           (is (-> (create-state ["# "] 4)
                   (active? [0 0 0 0])))
           (is-not (-> (create-state ["# "] 3)
                       (active? [1 0 0]))))}
  [state qube]
  (contains? (:active-cubes state) qube))

(defn get-neighbors
  {:test (fn []
           (is= (count (get-neighbors [2 0 0])) 26)
           (is= (count (get-neighbors [2 0 0 0])) 80))}
  [qube]
  (let [directions (if (= (count qube) 4)
                     (for [x (range -1 2)
                           y (range -1 2)
                           z (range -1 2)
                           w (range -1 2)
                           :when (not= [x y z w] [0 0 0 0])]
                       [x y z w])
                     (for [x (range -1 2)
                           y (range -1 2)
                           z (range -1 2)
                           :when (not= [x y z] [0 0 0])]
                       [x y z]))]
    (->> directions
         (map (fn [d] (map + d qube)))
         (set))))


(defn get-active-neighbors
  {:test (fn []
           (is= (-> (create-state ["###"
                                   "  #"
                                   "# #"]
                                  3)
                    (get-active-neighbors [2 0 0]))
                #{[1 0 0] [2 1 0]})
           (is= (-> (create-state ["###"
                                   "  #"
                                   "# #"]
                                  4)
                    (get-active-neighbors [2 0 0 0]))
                #{[1 0 0 0] [2 1 0 0]}))}
  [state qube]
  (->> (get-neighbors qube)
       (filter (fn [c] (active? state c)))
       (set)))

(defn active-in-the-next-generation?
  {:test (fn []
           ; If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active.
           ; Otherwise, the cube remains inactive.
           (is-not (-> (create-state ["###"] 3)
                       (active-in-the-next-generation? [0 0 0])))
           (is (-> (create-state ["###"] 3)
                   (active-in-the-next-generation? [1 0 0])))
           ; If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active.
           ; Otherwise, the cube becomes inactive.
           (is (-> (create-state ["###"] 3)
                   (active-in-the-next-generation? [1 0 0])))
           (is (-> (create-state ["###"
                                  " #"] 3)
                   (active-in-the-next-generation? [1 0 0]))))}
  [state cell]
  (let [live-neighbors (get-active-neighbors state cell)
        number-of-live-neighbors (count live-neighbors)]
    (or (and (active? state cell)
             (<= 2 number-of-live-neighbors 3))
        (and (not (active? state cell))
             (= number-of-live-neighbors 3)))))


(defn tick
  {:test (fn []
           (is= (-> (create-state test-input 3)
                    (tick)
                    (:active-cubes))
                #{[0 1 -1] [2 2 -1] [1 3 -1]
                  [0 1 0] [2 1 0] [1 2 0] [2 2 0] [1 3 0]
                  [0 1 1] [2 2 1] [1 3 1]}
                ;#{[0 0 -1] [2 1 -1] [1 2 -1]
                ;  [0 0 0] [2 0 0] [1 1 0] [2 1 0] [1 2 0]
                ;  [0 0 1] [2 1 1] [1 2 1]}
                ))}
  [state]
  (let [interesting-cells (->> (clojure.set/union (:active-cubes state)
                                                  (->> (:active-cubes state)
                                                       (map get-neighbors)
                                                       (apply clojure.set/union))))]
    (assoc state :active-cubes (->> interesting-cells
                                    (filter (fn [c] (active-in-the-next-generation? state c)))
                                    (set)))))

(deftest test-puzzle
         (is= (->> (create-state test-input 3)
                   ((apply comp (repeat 6 tick)))
                   (:active-cubes)
                   (count))
              112))

(deftest puzzle-a
         (is= (time (->> (create-state (get-puzzle-input) 3)
                         ((apply comp (repeat 6 tick)))
                         (:active-cubes)
                         (count)))
              ; "Elapsed time: 224.755917 msecs"
              237))

(deftest puzzle-b
         (is= (time (->> (create-state (get-puzzle-input) 4)
                         ((apply comp (repeat 6 tick)))
                         (:active-cubes)
                         (count)))
              ; "Elapsed time: 7201.658119 msecs"
              2448))





