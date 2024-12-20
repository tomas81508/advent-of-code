(ns advent-of-code.dec-2019.day-03
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]))


(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_03.txt") $
        (clojure.string/split-lines $)
        (map (fn [l] (clojure.string/split l #",")) $)))

(defn abs
  {:test (fn []
           (is= (abs -5) 5)
           (is= (abs 0) 0)
           (is= (abs 5) 5))}
  [x]
  (if (pos? x) x (- x)))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [1 3] [5 5])
                6))}
  [c1 c2]
  (->> (map - c1 c2)
       (map abs)
       (apply +)))

(defn create-state
  {:test (fn []
           (is= (create-state ["R10"])
                {[1 0] 1 [2 0] 1 [3 0] 1 [4 0] 1 [5 0] 1 [6 0] 1 [7 0] 1 [8 0] 1 [9 0] 1 [10 0] 1})
           (is= (create-state ["L2"])
                {[-1 0] 1 [-2 0] 1})
           (is= (create-state ["U2"])
                {[0 1] 1 [0 2] 1})
           (is= (create-state ["D2"])
                {[0 -1] 1 [0 -2] 1})
           (is= (create-state ["R2" "U2"]
                              ["U2" "R2"])
                {[1 0] 1 [2 0] 1 [2 1] 1 [2 2] 2
                 [0 1] 1 [0 2] 1 [1 2] 1})
           (is= (create-state ["L2" "D2"]
                              ["D2" "L2"])
                {[-1 0] 1 [-2 0] 1 [-2 -1] 1 [-2 -2] 2
                 [0 -1] 1 [0 -2] 1 [-1 -2] 1})
           (is= (create-state ["L2" "U1" "R1" "D1"]
                              ["U2" "L1" "D1" "R1"])
                {[-1 0] 1 [-2 0] 1 [-2 1] 1 [-1 1] 2 [0 1] 1 [0 2] 1 [-1 2] 1})
           (is= (create-state ["R8" "U5" "L5" "D3"]
                              ["U7" "R6" "D4" "L4"])
                {[4 3] 1 [1 0] 1 [8 4] 1 [2 3] 1 [6 7] 1 [8 3] 1 [0 6] 1 [3 3] 2 [6 3] 1 [0 5] 1 [3 4] 1 [3 0] 1
                 [6 6] 1 [5 3] 1 [4 7] 1 [6 5] 2 [8 0] 1 [5 7] 1 [8 2] 1 [1 7] 1 [6 4] 1 [8 1] 1 [0 3] 1 [8 5] 1
                 [0 7] 1 [5 5] 1 [2 7] 1 [4 5] 1 [7 0] 1 [0 2] 1 [2 0] 1 [0 4] 1 [3 7] 1 [7 5] 1 [5 0] 1 [6 0] 1
                 [3 5] 1 [3 2] 1 [0 1] 1 [4 0] 1}))}
  [& wire-paths]
  (->> wire-paths
       (map-indexed (fn [index wire-path] [index wire-path]))
       (reduce (fn [result [index wire-path]]
                 (loop [[current-x current-y] [0 0]
                        [path & paths] wire-path
                        result result]
                   (if-not path
                     result
                     (let [distance (read-string (subs path 1))
                           cells (condp = (first path)
                                   \U (for [y (range (inc current-y) (inc (+ current-y distance)))]
                                        [current-x y])
                                   \R (for [x (range (inc current-x) (inc (+ current-x distance)))]
                                        [x current-y])
                                   \D (for [y (range (dec current-y) (dec (- current-y distance)) -1)]
                                        [current-x y])
                                   \L (for [x (range (dec current-x) (dec (- current-x distance)) -1)]
                                        [x current-y]))]
                       (recur (last cells)
                              paths
                              (reduce (fn [a v]
                                        (if (contains? a v)
                                          (update a v conj index)
                                          (assoc a v #{index})))
                                      result
                                      cells))))))
               {})
       (reduce-kv (fn [a k v]
                    (assoc a k (count v)))
                  {})))

(defn print-map
  {:test (fn []
           (is= (-> (create-state ["R8" "U5" "L5" "D3"]
                                  ["U7" "R6" "D4" "L4"])
                    (print-map))
                (str "1111111..\n"
                     "1.....1..\n"
                     "1..111211\n"
                     "1..1..1.1\n"
                     "1.12111.1\n"
                     "1..1....1\n"
                     "1.......1\n"
                     "o11111111")))}
  [state]
  (let [screen-size 15
        xs (->> (keys state) (map first))
        min-x (max (apply min xs) (- screen-size))
        max-x (min (apply max xs) screen-size)
        ys (->> (keys state) (map second))
        min-y (max (apply min ys) (- screen-size))
        max-y (min (apply max ys) screen-size)]
    (->> (for [y (range max-y (dec min-y) -1)
               x (range min-x (inc max-x))]
           [x y])
         (partition (inc (- max-x min-x)))
         (map (fn [cells]
                (reduce (fn [result cell]
                          (str result
                               (if-let [value (get state cell)]
                                 value
                                 (if (= cell [0 0]) "o" "."))))
                        ""
                        cells)))
         (clojure.string/join "\n"))))

(defn find-min-distance
  {:test (fn []
           (is= (-> (create-state ["R8" "U5" "L5" "D3"]
                                  ["U7" "R6" "D4" "L4"])
                    (find-min-distance))
                6))}
  [state]
  (->> state
       (seq)
       (filter (fn [[_ value]] (> value 1)))
       (map (fn [[cell _]] (manhattan-distance cell [0 0])))
       (apply min)))

(deftest puzzle-a
  (is= (time (->> (get-puzzle-input)
                  (apply create-state)
                  (find-min-distance)))
       ; "Elapsed time: 580.560342 msecs"
       375))

;;; PART 2

(defn create-state-2
  {:test (fn []
           (is= (create-state-2 ["L2" "U1" "R1" "D1" "U1"]
                                ["U2" "L1" "D1" "R1"])
                {[-1 0] {0 1}
                 [-2 0] {0 2}
                 [-2 1] {0 3}
                 [-1 1] {0 4 1 4}
                 [0 1]  {1 1}
                 [0 2]  {1 2}
                 [-1 2] {1 3}}))}
  [& wire-paths]
  (->> wire-paths
       (map-indexed (fn [wire-id wire-path] [wire-id wire-path]))
       (reduce (fn [result [wire-id wire-path]]
                 (loop [[current-x current-y] [0 0]
                        steps 0
                        [path & paths] wire-path
                        result result]
                   (if-not path
                     result
                     (let [distance (read-string (subs path 1))
                           cells (condp = (first path)
                                   \U (for [y (range (inc current-y) (inc (+ current-y distance)))]
                                        [current-x y])
                                   \R (for [x (range (inc current-x) (inc (+ current-x distance)))]
                                        [x current-y])
                                   \D (for [y (range (dec current-y) (dec (- current-y distance)) -1)]
                                        [current-x y])
                                   \L (for [x (range (dec current-x) (dec (- current-x distance)) -1)]
                                        [x current-y]))]
                       (recur (last cells)
                              (+ steps (count cells))
                              paths
                              (->> cells
                                   (map-indexed (fn [index cell] [(+ steps index 1) cell]))
                                   (reduce (fn [a [step cell]]
                                             (if (get-in a [cell wire-id])
                                               a
                                               (assoc-in a [cell wire-id] step)))
                                           result)))))))
               {})))

(defn find-min-wire-total-length
  {:test (fn []
           (is= (-> (create-state-2 ["R8" "U5" "L5" "D3"]
                                    ["U7" "R6" "D4" "L4"])
                    (find-min-wire-total-length))
                {:distance          11
                 :wire-total-length 30}))}
  [state]
  (->> state
       (seq)
       (filter (fn [[_ data]] (> (count (keys data)) 1)))
       (map (fn [[cell data]] {:distance          (manhattan-distance cell [0 0])
                               :wire-total-length (apply + (vals data))}))
       (sort-by :wire-total-length)
       (first)))

(deftest puzzle-b
  (is= (time (->> (get-puzzle-input)
                  (apply create-state-2)
                  (find-min-wire-total-length)))
       ; "Elapsed time: 682.542272 msecs"
       {:distance          1813
        :wire-total-length 14746}))
