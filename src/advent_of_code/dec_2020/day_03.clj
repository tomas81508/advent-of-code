(ns advent-of-code.dec-2020.day-03
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_03.txt")
       (clojure.string/split-lines)))

(def test-input ["..##......."
                 "#...#...#.."
                 ".#....#..#."
                 "..#.#...#.#"
                 ".#...##..#."
                 "..#.##....."
                 ".#.#.#....#"
                 ".#........#"
                 "#.##...#..."
                 "#...##....#"
                 ".#..#...#.#"])

(defn create-state
  {:test (fn []
           (is= (create-state ["..#.."
                               "##..."])
                {:trees  #{[2 0] [0 1] [1 1]}
                 :width  5
                 :length 2}))}
  [strings]
  {:trees  (->> strings
                (map-indexed (fn [y s] [y s]))
                (reduce (fn [trees [y s]]
                          (->> s
                               (map-indexed (fn [x l] [x l]))
                               (filter (fn [[_ l]] (= l \#)))
                               (reduce (fn [trees [x _]]
                                         (conj trees [x y]))
                                       trees)))
                        #{}))
   :width  (count (first strings))
   :length (count strings)})

(defn tree?
  {:test (fn []
           (let [state (create-state ["..#" "##."])]
             (is (tree? state [2 0]))
             (is-not (tree? state [0 0]))
             (is (tree? state [0 1]))
             (is-not (tree? state [2 1]))
             (is (tree? state [5 0]))
             (is-not (tree? state [4 0]))))}
  [state [x y]]
  (contains? (:trees state) [(mod x (:width state)) y]))

(defn number-of-found-trees
  {:test (fn []
           (is= (-> (create-state test-input)
                    (number-of-found-trees [3 1]))
                7))}
  [state [dx dy]]
  (->> (range (:length state))
       (map (fn [y] [(* y (/ dx dy)) y]))
       (filter (fn [position] (tree? state position)))
       (count)))

(deftest puzzle-a
         (is= (let [state (create-state (get-puzzle-input))]
                (number-of-found-trees state [3 1]))
              205))

(deftest puzzle-b
         (is= (let [state (create-state (get-puzzle-input))
                    slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
                (->> slopes
                     (map (fn [slope] (number-of-found-trees state slope)))
                     (apply *)))
              3952146825))

