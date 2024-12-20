(ns advent-of-code.dec-2020.day-20
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(defn create-state
  {:test (fn []
           (is= (create-state "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###")
                {"2311" {:id            "2311"
                         :border-pixels [[2 0] [3 0] [5 0] [8 0]
                                         [0 1]
                                         [0 2]
                                         [0 3] [9 3]
                                         [0 4]
                                         [0 5] [9 5]
                                         [9 6]
                                         [0 8]
                                         [2 9] [3 9] [4 9] [7 9] [8 9] [9 9]]}}))}
  [input]
  (as-> input $
        (clojure.string/split $ #"\n\n")
        (map (fn [lines] (clojure.string/split lines #"\n")) $)
        (map (fn [[header & square]]
               {:id            (subs header 5 9)
                :border-pixels (->> (concat (->> (first square)
                                                 (seq)
                                                 (map-indexed (fn [index item]
                                                                (when (= item \#) [index 0]))))
                                            (->> square
                                                 (drop 1)
                                                 (drop-last)
                                                 (map-indexed (fn [y line]
                                                                [(when (= (first line) \#)
                                                                   [0 (inc y)])
                                                                 (when (= (last line) \#)
                                                                   [9 (inc y)])]))
                                                 (apply concat))
                                            (->> (last square)
                                                 (seq)
                                                 (map-indexed (fn [index item]
                                                                (when (= item \#) [index 9])))))
                                    (remove (fn [it] (or (nil? it) (= it (list))))))})
             $)
        (reduce (fn [a v]
                  (assoc a (:id v) v))
                {}
                $)))

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2020/day_20.txt")
      (create-state)))


(defn get-test-input []
  (-> (slurp "src/advent_of_code/dec_2020/day_20_test_data.txt")
      (create-state)))


(def test-state (get-test-input))

(defn get-tile
  [state tile-id]
  (get state tile-id))

(defn change-tile
  {:test (fn []
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 0)
                {:border-pixels [[0 0] [1 0]]})
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 1)
                {:border-pixels [[9 0] [9 1]]})
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 2)
                {:border-pixels [[9 9] [8 9]]})
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 3)
                {:border-pixels [[0 9] [0 8]]})
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 4)
                {:border-pixels [[9 0] [8 0]]})
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 5)
                {:border-pixels [[9 9] [9 8]]})
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 6)
                {:border-pixels [[0 9] [1 9]]})
           (is= (change-tile {:border-pixels [[0 0] [1 0]]} 7)
                {:border-pixels [[0 0] [0 1]]}))}
  [tile n]
  (if (zero? n)
    tile
    (update tile :border-pixels
            (fn [pixels]
              (case n
                1 (map (fn [[x y]] [(- 9 y) x]) pixels)
                2 (map (fn [[x y]] [(- 9 x) (- 9 y)]) pixels)
                3 (map (fn [[x y]] [y (- 9 x)]) pixels)
                4 (map (fn [[x y]] [(- 9 x) y]) pixels)
                5 (map (fn [[x y]] [(- 9 y) (- 9 x)]) pixels)
                6 (map (fn [[x y]] [x (- 9 y)]) pixels)
                7 (map (fn [[x y]] [y x]) pixels))))))

(def change-tile (memoize change-tile))

(defn get-left-border
  {:test (fn []
           (is= (get-left-border {:border-pixels [[0 0] [9 0] [0 9] [9 9] [0 4]]})
                [0 4 9]))}
  [tile]
  (->> (:border-pixels tile)
       (filter (fn [[x _]] (zero? x)))
       (map second)
       (sort)))

(defn get-right-border
  {:test (fn []
           (is= (get-right-border {:border-pixels [[0 0] [9 0] [0 9] [9 9] [9 4]]})
                [0 4 9]))}
  [tile]
  (->> (:border-pixels tile)
       (filter (fn [[x _]] (= x 9)))
       (map second)
       (sort)))


(defn match
  {:test (fn []
           (is= (let [tile1 (get-tile test-state "1951")
                      tile2 (get-tile test-state "2311")]
                  (match tile1 tile2))
                [[0 0] [6 6]])
           (is= (let [tile1 (get-tile test-state "2311")
                      tile2 (get-tile test-state "3079")]
                  (match tile1 tile2))
                [[0 6] [6 0]])
           (is= (let [tile1 (get-tile test-state "1951")
                      tile2 (get-tile test-state "1171")]
                  (match tile1 tile2))
                []))}
  [tile1 tile2]
  (->> (for [n1 (range 8)
             n2 (range 8)]
         [n1 n2])
       (map (fn [[n1 n2]]
              (let [tile1-version (change-tile tile1 n1)
                    tile2-version (change-tile tile2 n2)
                    right-border (get-right-border tile1-version)
                    left-border (get-left-border tile2-version)]
                (when (= right-border left-border)
                  [n1 n2]))))
       (remove nil?)))

(defn find-all-matches
  [state]
  (reduce (fn [state tile]
            (let [matches (reduce (fn [a other-tile]
                                    (let [match-orientations (match tile other-tile)]
                                      (if-not (empty? match-orientations)
                                        (assoc a (:id other-tile) match-orientations)
                                        a)))
                                  {}
                                  (remove (fn [t] (= t tile)) (vals state)))]
              (assoc-in state [(:id tile) :matches] matches)))
          state
          (vals state)))

(def test-state-with-matches (find-all-matches test-state))

(defn get-corner-tiles
  {:test (fn []
           (is= (get-corner-tiles test-state)
                #{"1951" "3079" "2971" "1171"}))}
  [state]
  (->> (find-all-matches state)
       (vals)
       (filter (fn [tile] (= 2 (count (keys (:matches tile))))))
       (map :id)
       (set)))

(deftest puzzle-a
  (is= (time (->> (get-corner-tiles (get-puzzle-input))
                  (map read-string)
                  (apply *)))
       ; "Elapsed time: 17050.955188 msecs"
       13983397496713))

;;;;;; Part b

(defn create-state-2
  {:test (fn []
           (is= (create-state "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###")
                {"2311" {:id            "2311"
                         :border-pixels [[2 0] [3 0] [5 0] [8 0]
                                         [0 1]
                                         [0 2]
                                         [0 3] [9 3]
                                         [0 4]
                                         [0 5] [9 5]
                                         [9 6]
                                         [0 8]
                                         [2 9] [3 9] [4 9] [7 9] [8 9] [9 9]]}}))}
  [input]
  (as-> input $
        (clojure.string/split $ #"\n\n")
        (map (fn [lines] (clojure.string/split lines #"\n")) $)
        (map (fn [[header & square]]
               {:id            (subs header 5 9)
                :pixels        (->> square
                                    (map-indexed (fn [y row]
                                                   (->> row
                                                        (map-indexed (fn [x c]
                                                                       (when (= c \#) [x y])))
                                                        (remove nil?))))
                                    (concat))
                :border-pixels (->> (concat (->> (first square)
                                                 (seq)
                                                 (map-indexed (fn [index item]
                                                                (when (= item \#) [index 0]))))
                                            (->> square
                                                 (drop 1)
                                                 (drop-last)
                                                 (map-indexed (fn [y line]
                                                                [(when (= (first line) \#)
                                                                   [0 (inc y)])
                                                                 (when (= (last line) \#)
                                                                   [9 (inc y)])]))
                                                 (apply concat))
                                            (->> (last square)
                                                 (seq)
                                                 (map-indexed (fn [index item]
                                                                (when (= item \#) [index 9])))))
                                    (remove (fn [it] (or (nil? it) (= it (list))))))})
             $)
        (reduce (fn [a v]
                  (assoc a (:id v) v))
                {}
                $)))











