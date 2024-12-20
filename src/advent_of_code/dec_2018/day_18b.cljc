(ns advent-of-code.dec-2018.day-18b
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]))

(defn create-state
  {:test (fn []
           (is= (create-state [".#."
                               ".#|"
                               ".|."])
                {[1 0] :lumberyard
                 [1 1] :lumberyard
                 [2 1] :trees
                 [1 2] :trees}))}
  [strings]
  (->> strings
       (map-indexed (fn [y string] [y string]))
       (reduce (fn [state [y string]]
                 (->> string
                      (map-indexed (fn [x letter] [x letter]))
                      (reduce (fn [state [x letter]]
                                (if (= letter \.)
                                  state
                                  (assoc state [x y] (if (= letter \#)
                                                       :lumberyard
                                                       :trees))))
                              state)))
               {})))

(defn get-neighbours
  {:test (fn []
           (is= (get-neighbours [4 4] 10)
                #{[3 3] [4 3] [5 3]
                  [3 4] [5 4]
                  [3 5] [4 5] [5 5]})
           (is= (get-neighbours [0 0] 10)
                #{[1 0] [1 1] [0 1]})
           (is= (get-neighbours [49 49] 50)
                #{[48 49] [48 48] [49 48]}))}
  [[cx cy :as cell] size]
  (->> (for [x (range (max 0 (dec cx)) (min size (inc (inc cx))))
             y (range (max 0 (dec cy)) (min size (inc (inc cy))))]
         [x y])
       (remove (fn [c] (= c cell)))
       (into #{})))

(def get-neighbours-memoized (memoize get-neighbours))

(defn get-boundary
  {:test (fn []
           (is= (get-boundary [[3 3] [3 4]] 10)
                #{[2 2] [3 2] [4 2]
                  [2 3] [4 3]
                  [2 4] [4 4]
                  [2 5] [3 5] [4 5]})
           (is= (get-boundary [[9 9]] 10)
                #{[8 9] [8 8] [9 8]}))}
  [cells size]
  (as-> cells $
        (map (fn [c] (get-neighbours-memoized c size)) $)
        (apply clojure.set/union $)
        (clojure.set/difference $ (set cells))))

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
                    (tick 10))
                (create-state [".......##."
                               "......|###"
                               ".|..|...#."
                               "..|#||...#"
                               "..##||.|#|"
                               "...#||||.."
                               "||...|||.."
                               "|||||.||.|"
                               "||||||||||"
                               "....||..|."]))
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
                    (tick 10) (tick 10))
                (create-state [".......#.."
                               "......|#.."
                               ".|.|||...."
                               "..##|||..#"
                               "..###|||#|"
                               "...#|||||."
                               "|||||||||."
                               "||||||||||"
                               "||||||||||"
                               ".|||||||||"]))
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
                    (tick 10) (tick 10) (tick 10) (tick 10) (tick 10) (tick 10) (tick 10) (tick 10) (tick 10) (tick 10))
                (create-state [".||##....."
                               "||###....."
                               "||##......"
                               "|##.....##"
                               "|##.....##"
                               "|##....##|"
                               "||##.####|"
                               "||#####|||"
                               "||||#|||||"
                               "||||||||||"])))}
  [state size]
  (let [open-ground (get-boundary (keys state) size)
        result (reduce (fn [result cell]
                         (let [neighbours (get-neighbours-memoized cell size)]
                           (if (>= (->> neighbours
                                        (filter (fn [c] (= (get state c) :trees)))
                                        (count))
                                   3)
                             (assoc result cell :trees)
                             result)))
                       state
                       open-ground)]
    (reduce-kv (fn [result cell v]
                 (let [neighbours (get-neighbours-memoized cell size)]
                   (cond (and (= v :trees)
                              (>= (->> neighbours
                                       (filter (fn [c] (= (get state c) :lumberyard)))
                                       (count))
                                  3))
                         (assoc result cell :lumberyard)

                         (and (= v :lumberyard)
                              (or (not (some (fn [c] (= (get state c) :lumberyard)) neighbours))
                                  (not (some (fn [c] (= (get state c) :trees)) neighbours))))
                         (dissoc result cell)

                         :else
                         result)))
               result
               state)))


(def memoized-tick (memoize tick))


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
           (tick 10) (tick 10) (tick 10) (tick 10) (tick 10)
           (tick 10) (tick 10) (tick 10) (tick 10) (tick 10)
           (get-resource-value))
       1147))

(defn solve-puzzle-a []
  (as-> (slurp "src/advent_of_code/dec_2018/day_18.txt") $
        (clojure.string/split-lines $)
        (create-state $)
        (reduce (fn [state i] (memoized-tick state 50))
                $
                (range 10))
        (get-resource-value $))
  ; => 621205
  )

(defn solve-puzzle-b []
  (time (as-> (slurp "src/advent_of_code/dec_2018/day_18.txt") $
              (clojure.string/split-lines $)
              (create-state $)
              (loop [state $
                     i 0
                     results {}
                     ticks-left nil]
                (if ticks-left
                  (if (zero? ticks-left)
                    state
                    (recur (memoized-tick state 50)
                           (inc i)
                           nil
                           (dec ticks-left)))
                  (let [h (hash state)]
                    (if (or (contains? results h) (> i 10000))
                      (let [index (get results h)
                            cycle-length (- i index)
                            ticks-left (mod (- 1000000000 i) cycle-length)]
                        (recur (memoized-tick state 50)
                               (inc i)
                               nil
                               (dec ticks-left)))
                      (recur (memoized-tick state 50)
                             (inc i)
                             (assoc results h i)
                             nil)))))
              (get-resource-value $)))
  ; => 228490
  )