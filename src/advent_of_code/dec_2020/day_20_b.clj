(ns advent-of-code.dec-2020.day-20-b
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.collections :refer [seq-contains?]]
            [advent-of-code.math :refer [floor]]))

(defn create-state
  {:test (fn []
           (is= (create-state "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###")
                {"2311" {:id     "2311"
                         :pixels [[2 0] [3 0] [5 0] [8 0]
                                  [0 1] [1 1] [4 1]
                                  [0 2] [4 2] [5 2] [8 2]
                                  [0 3] [1 3] [2 3] [3 3] [5 3] [9 3]
                                  [0 4] [1 4] [3 4] [4 4] [6 4] [7 4] [8 4]
                                  [0 5] [1 5] [5 5] [7 5] [8 5] [9 5]
                                  [1 6] [3 6] [5 6] [8 6] [9 6]
                                  [2 7] [7 7]
                                  [0 8] [1 8] [2 8] [6 8] [8 8]
                                  [2 9] [3 9] [4 9] [7 9] [8 9] [9 9]]}}))}
  [input]
  (as-> input $
        (clojure.string/split $ #"\n\n")
        (map (fn [lines] (clojure.string/split lines #"\n")) $)
        (map (fn [[header & square]]
               {:id     (subs header 5 9)
                :pixels (->> square
                             (map-indexed (fn [y row]
                                            (->> row
                                                 (map-indexed (fn [x c]
                                                                (when (= c \#) [x y])))
                                                 (remove nil?))))
                             (apply concat))})
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
(def state (get-puzzle-input))

(defn get-tile
  [state tile-id]
  (get state tile-id))

(defn change-tile
  {:test (fn []
           (is= (change-tile {:pixels [[0 0] [1 0]]} :i)
                {:pixels [[0 0] [1 0]]})
           (is= (change-tile {:pixels [[0 0] [1 0]]} :r)
                {:pixels [[9 0] [9 1]]})
           (is= (change-tile {:pixels [[0 0] [1 0]]} :rr)
                {:pixels [[9 9] [8 9]]})
           (is= (change-tile {:pixels [[0 0] [1 0]]} :rrr)
                {:pixels [[0 9] [0 8]]})
           (is= (change-tile {:pixels [[0 0] [1 0]]} :f)
                {:pixels [[9 0] [8 0]]})
           (is= (change-tile {:pixels [[0 0] [1 0]]} :fr)
                {:pixels [[9 9] [9 8]]})
           (is= (change-tile {:pixels [[0 0] [1 0]]} :frr)
                {:pixels [[0 9] [1 9]]})
           (is= (change-tile {:pixels [[0 0] [1 0]]} :frrr)
                {:pixels [[0 0] [0 1]]}))}
  [tile n]
  (if (= n :i)
    tile
    (update tile :pixels
            (fn [pixels]
              (case n
                :r (map (fn [[x y]] [(- 9 y) x]) pixels)
                :rr (map (fn [[x y]] [(- 9 x) (- 9 y)]) pixels)
                :rrr (map (fn [[x y]] [y (- 9 x)]) pixels)
                :f (map (fn [[x y]] [(- 9 x) y]) pixels)
                :fr (map (fn [[x y]] [(- 9 y) (- 9 x)]) pixels)
                :frr (map (fn [[x y]] [x (- 9 y)]) pixels)
                :frrr (map (fn [[x y]] [y x]) pixels))))))

(def change-tile (memoize change-tile))

(defn get-left-border
  {:test (fn []
           (is= (get-left-border {:pixels [[0 0] [9 0] [0 9] [9 9] [0 4]]})
                [0 4 9]))}
  [tile]
  (->> (:pixels tile)
       (filter (fn [[x _]] (zero? x)))
       (map second)
       (sort)))

(defn get-right-border
  {:test (fn []
           (is= (get-right-border {:pixels [[0 0] [9 0] [0 9] [9 9] [9 4]]})
                [0 4 9]))}
  [tile]
  (->> (:pixels tile)
       (filter (fn [[x _]] (= x 9)))
       (map second)
       (sort)))

(defn match
  {:test (fn []
           (is= (let [tile1 (get-tile test-state "1951")
                      tile2 (get-tile test-state "2311")]
                  (match tile1 tile2))
                [[:i :i] [:frr :frr]])
           (is= (let [tile1 (get-tile test-state "2311")
                      tile2 (get-tile test-state "3079")]
                  (match tile1 tile2))
                [[:i :frr] [:frr :i]])
           (is= (let [tile1 (get-tile test-state "1951")
                      tile2 (get-tile test-state "1171")]
                  (match tile1 tile2))
                []))}
  [tile1 tile2]
  (let [orientations [:i :r :rr :rrr :f :fr :frr :frrr]]
    (->> (for [n1 orientations
               n2 orientations]
           [n1 n2])
         (map (fn [[n1 n2]]
                (let [tile1-version (change-tile tile1 n1)
                      tile2-version (change-tile tile2 n2)
                      right-border (get-right-border tile1-version)
                      left-border (get-left-border tile2-version)]
                  (when (= right-border left-border)
                    [n1 n2]))))
         (remove nil?))))

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
(defonce state-with-matches (find-all-matches state))

(defn get-corner-tiles
  {:test (fn []
           (is= (get-corner-tiles test-state-with-matches)
                #{"1951" "3079" "2971" "1171"}))}
  [state]
  (->> state
       (vals)
       (filter (fn [tile] (= 2 (count (keys (:matches tile))))))
       (map :id)
       (set)))

(def rotate-right
  {:i    :r
   :r    :rr
   :rr   :rrr
   :rrr  :i
   :f    :fr
   :fr   :frr
   :frr  :frrr
   :frrr :f})

(def rotate-left (clojure.set/map-invert rotate-right))

(defn get-initial-orientation
  {:test (fn []
           (is= (get-initial-orientation {:matches {"2311" [[:i :i] [:frr :frr]] "2729" [[:r :r] [:fr :fr]]}})
                :r))}
  [tile]
  (let [matching-orientations (->> (:matches tile)
                                   (vals)
                                   (map ffirst)
                                   (set))]
    (case matching-orientations
      #{:i :r} :r
      #{:r :rr} :rr
      #{:rr :rrr} :rrr
      #{:rrr :i} :i
      #{:f :fr} :fr
      #{:fr :frr} :frr
      #{:frr :frrr} :frrr
      #{:frrr :f} :f)))

(defn puzzle-strategy
  {:test (fn []
           (is= (puzzle-strategy test-state-with-matches "1951")
                {[2 2] {:id "1171" :orientation :rrr}
                 [0 0] {:id "1951" :orientation :r}
                 [1 0] {:id "2729" :orientation :r}
                 [1 1] {:id "1427" :orientation :r}
                 [0 2] {:id "3079" :orientation :frrr}
                 [2 0] {:id "2971" :orientation :r}
                 [2 1] {:id "1489" :orientation :r}
                 [1 2] {:id "2473" :orientation :rr}
                 [0 1] {:id "2311" :orientation :r}}))}
  ([state]
   (puzzle-strategy state (first (get-corner-tiles state))))
  ([state corner-tile-id]
   (let [corner-tile (get-tile state corner-tile-id)
         size (floor (Math/sqrt (count (vals state))))]
     (->> (for [y (range size)
                x (range size)
                :when (not= [x y] [0 0])]
            [x y])
          (reduce (fn [a [x y]]
                    (let [[puzzled-tile puzzled-orientation] ; :rrr
                          (let [puzzled-tile (if (pos? x) (get a [(dec x) y]) (get a [x (dec y)]))]
                            [puzzled-tile (get puzzled-tile :orientation)])
                          neighbors (get-in puzzled-tile [:tile :matches])]
                      (assoc a [x y] (as-> neighbors $
                                           (seq $)
                                           (filter (fn [[_ matches]]
                                                     (some (fn [[o _]]
                                                             (if (pos? x)
                                                               (= o puzzled-orientation)
                                                               (= o (rotate-left puzzled-orientation))))
                                                           matches))
                                                   $)
                                           (first $)
                                           {:tile        (get-tile state (first $))
                                            :orientation (let [orientation (->> (second $)
                                                                                (filter (fn [[o _]]
                                                                                          (if (pos? x)
                                                                                            (= o puzzled-orientation)
                                                                                            (= o (rotate-left puzzled-orientation)))))
                                                                                (first)
                                                                                (second))]
                                                           (if (pos? x)
                                                             orientation
                                                             (rotate-right orientation)))}))))
                  {[0 0] {:tile        corner-tile
                          :orientation (get-initial-orientation corner-tile)}})
          (reduce-kv (fn [a k v]
                       (assoc a k {:id          (get-in v [:tile :id])
                                   :orientation (:orientation v)}))
                     {})))))


(def test-configuration (puzzle-strategy test-state-with-matches "1951"))
(def configuration (puzzle-strategy state-with-matches))


(defn change-image
  [pixels n]
  (let [size (inc (->> pixels
                       (map first)
                       (apply max)))]
    (if (= n :i)
      pixels
      (case n
        :r (map (fn [[x y]] [(- size y) x]) pixels)
        :rr (map (fn [[x y]] [(- size x) (- size y)]) pixels)
        :rrr (map (fn [[x y]] [y (- size x)]) pixels)
        :f (map (fn [[x y]] [(- size x) y]) pixels)
        :fr (map (fn [[x y]] [(- size y) (- size x)]) pixels)
        :frr (map (fn [[x y]] [x (- size y)]) pixels)
        :frrr (map (fn [[x y]] [y x]) pixels)))))

(defn puzzle-the-tiles-without-borders
  [state configuration]
  (let [tile-size 8]
    (->> (keys configuration)
         (reduce (fn [a c]
                   (let [tile-orientation-data (get configuration c)
                         tile (change-tile (get-tile state (:id tile-orientation-data))
                                           (:orientation tile-orientation-data))]
                     (concat a
                             (->> (:pixels tile)
                                  (remove (fn [[x y]] (or (= x 0) (= x 9) (= y 0) (= y 9))))
                                  (map (fn [c] (map - c [1 1])))
                                  (reduce (fn [a [x y]]
                                            (conj a [(+ x (* (first c) tile-size))
                                                     (+ y (* (second c) tile-size))]))
                                          [])))))
                 [])
         (sort-by (juxt second first)))))

(defn cells->string
  [cells]
  (let [size (inc (->> cells
                       (map first)
                       (apply max)))]
    (->> (for [y (range size)
               x (range size)]
           [x y])
         (partition (* 8 size))
         (map (fn [part-rows]
                (->> part-rows
                     (partition size)
                     (map (fn [row-cells]
                            (->> row-cells
                                 (partition 8)
                                 (map (fn [row-part-cells]
                                        (reduce (fn [a c]
                                                  (str a (if (seq-contains? cells c) "#" ".")))
                                                ""
                                                row-part-cells)))
                                 ;(clojure.string/join " ")
                                 (clojure.string/join ""))))
                     (clojure.string/join "\n"))))
         ;(clojure.string/join "\n\n")
         (clojure.string/join "\n")
         )))

(def test-picture (puzzle-the-tiles-without-borders test-state test-configuration))
(def picture (puzzle-the-tiles-without-borders state configuration))

(def monster (->> ["                  # "
                   "#    ##    ##    ###"
                   " #  #  #  #  #  #   "]
                  (map-indexed (fn [y rows]
                                 (->> rows
                                      (map-indexed (fn [x c] (when (= c \#) [x y])))
                                      (remove nil?))))
                  (apply concat)))

(defn sea-monster
  {:test (fn []
           (is= (sea-monster (puzzle-the-tiles-without-borders test-state test-configuration))
                [[1 16] [2 2]]))}
  [pixels]
  (let [size (inc (->> pixels
                       (map first)
                       (apply max)))
        pixels (set pixels)]
    (->> (for [x (range (- size 19))
               y (range (- size 3))]
           [x y])
         (filter (fn [init-pixel]
                   (every? (fn [m]
                             (let [p (map + m init-pixel)]
                               (contains? pixels p)))
                           monster))))))

(deftest puzzle-b
  (is= (let [number-of-sea-monsters (->> [:i :r :rr :rrr :f :fr :frr :frrr]
                                         (map (fn [orientation]
                                                (let [p (change-image picture orientation)]
                                                  (count (sea-monster p)))))
                                         (apply max))]
         (- (count picture) (* number-of-sea-monsters (count monster))))
       2424))








