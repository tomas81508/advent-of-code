(ns advent-of-code.dec-2018.day-18a
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]))

(defn create-state
  {:test (fn []
           (is= (create-state [".#."
                               ".#|"
                               ".|."])

                {[0 0] :open-ground
                 [1 0] :lumberyard
                 [2 0] :open-ground
                 [0 1] :open-ground
                 [1 1] :lumberyard
                 [2 1] :trees
                 [0 2] :open-ground
                 [1 2] :trees
                 [2 2] :open-ground}))}
  [strings]
  (->> strings
       (map-indexed (fn [y string] [y string]))
       (reduce (fn [state [y string]]
                 (->> string
                      (map-indexed (fn [x letter] [x letter]))
                      (reduce (fn [state [x letter]]
                                (assoc state [x y] (cond (= letter \.) :open-ground
                                                         (= letter \#) :lumberyard
                                                         :else :trees)))
                              state)))
               {})))

(defn get-neighbours
  {:test (fn []
           (is= (get-neighbours [4 4])
                #{[3 3] [4 3] [5 3]
                  [3 4] [5 4]
                  [3 5] [4 5] [5 5]})
           (is= (get-neighbours [0 0])
                #{[1 0] [1 1] [0 1]})
           (is= (get-neighbours [50 50])
                #{[49 50] [49 49] [50 49]}))}
  [[cx cy :as cell]]
  (->> (for [x (range (max 0 (dec cx)) (min 51 (inc (inc cx))))
             y (range (max 0 (dec cy)) (min 51 (inc (inc cy))))]
         [x y])
       (remove (fn [c] (= c cell)))
       (into #{})))

(defn tick
  {:test (fn []
           (is= (-> (create-state [".#.#...|#."
                                   ".....#|##|"
                                   ".|..|...#."
                                   "..|#.....#"
                                   "#.#|||#|#|"
                                   "...#.||..."
                                   ".|....|..."
                                   "||...#|.#|"
                                   "|.||||..|."
                                   "...#.|..|."])
                    (tick))
                (create-state [".......##."
                               "......|###"
                               ".|..|...#."
                               "..|#||...#"
                               "..##||.|#|"
                               "...#||||.."
                               "||...|||.."
                               "|||||.||.|"
                               "||||||||||"
                               "....||..|."])))}
  [state]
  (reduce-kv (fn [next-state k v]
               (let [neighbours (get-neighbours k)
                     neighbour-values (map (fn [c] (get state c)) neighbours)]
                 (cond (and (= v :open-ground)
                            (>= (->> neighbour-values
                                     (filter (fn [v] (= v :trees)))
                                     (count))
                                3))
                       (assoc next-state k :trees)

                       (and (= v :trees)
                            (>= (->> neighbour-values
                                     (filter (fn [v] (= v :lumberyard)))
                                     (count))
                                3))
                       (assoc next-state k :lumberyard)

                       (and (= v :lumberyard)
                            (or (not (some (fn [v] (= v :lumberyard)) neighbour-values))
                                (not (some (fn [v] (= v :trees)) neighbour-values))))
                       (assoc next-state k :open-ground)

                       :else
                       next-state)))
             state
             state))

(defn get-resource-value
  [state]
  (* (->> (vals state)
          (filter (fn [x] (= x :trees)))
          (count))
     (->> (vals state)
          (filter (fn [x] (= x :lumberyard)))
          (count))))

(deftest from-spec
  (is= (-> (create-state [".#.#...|#."
                          ".....#|##|"
                          ".|..|...#."
                          "..|#.....#"
                          "#.#|||#|#|"
                          "...#.||..."
                          ".|....|..."
                          "||...#|.#|"
                          "|.||||..|."
                          "...#.|..|."])
           (tick) (tick) (tick) (tick) (tick)
           (tick) (tick) (tick) (tick) (tick)
           (get-resource-value))
       1147))

(defn solve-puzzle-a []
  (as-> (slurp "src/advent_of_code/dec_2018/day_18.txt") $
        (clojure.string/split-lines $)
        (create-state $)
        (reduce (fn [state i] (tick state))
                $
                (range 10))
        (get-resource-value $))
  ; => 621205
  )
