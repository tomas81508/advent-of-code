(ns advent-of-code.dec-2021.day-20
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]))

;; Together with Mattias Liljeström

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_20_input.txt"))

(def test-puzzle-input "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###")

(defn create-enhancement-algorithm
  [input]
  (->> input
       (string/split-lines)
       (first)
       (map-indexed (fn [index pixel] [index pixel]))
       (into {})))

(def test-enhancement-algorithm
  (create-enhancement-algorithm test-puzzle-input))

(def enhancement-algorithm
  (create-enhancement-algorithm puzzle-input))

(defn create-initial-state
  {:test (fn []
           (is= (create-initial-state test-puzzle-input)
                {:pixels     #{[0 0] [3 0] [0 1] [0 2] [1 2] [4 2] [2 3] [2 4] [3 4] [4 4]}
                 :background :dark}))}
  [input]
  {:pixels     (->> input
                    (string/split-lines)
                    (drop 2)
                    (into [])
                    (reduce-kv (fn [a y row]
                                 (->> (seq row)
                                      (vec)
                                      (reduce-kv (fn [a x pixel]
                                                   (if (= pixel \#)
                                                     (conj a [x y])
                                                     a))
                                                 a)))
                               #{}))
   :background :dark})

(def unit-square (for [y (range -1 2) x (range -1 2)] [x y]))

;; Since
(= (get enhancement-algorithm 0) \#)
(= (get enhancement-algorithm 511) \.)
;; the background will alternate between light and darkness

(defn get-focus-elements [pos]
  (->> unit-square
       (map (fn [d] (map + pos d)))))

(defn next-background
  {:test (fn []
           (is= (next-background :dark test-enhancement-algorithm)
                :dark)
           (is= (next-background :dark enhancement-algorithm)
                :light)
           (is= (next-background :light enhancement-algorithm)
                :dark))}
  [background enhancement-algorithm]
  (if (= background :dark)
    (if (= (get enhancement-algorithm 0) \#) :light :dark)
    (if (= (get enhancement-algorithm 511) \#) :light :dark)))

(defn light?
  {:test (fn []
           (is (light? (create-initial-state test-puzzle-input)
                       test-enhancement-algorithm
                       [2 2]))
           (is-not (light? {:pixels #{} :background :light}
                           enhancement-algorithm
                           [0 0])))}
  [state enhancement-algorithm pos]
  (as-> (get-focus-elements pos) $
        (map (fn [p] (cond (and (= (:background state) :dark) (contains? (:pixels state) p)) 1
                           (= (:background state) :dark) 0
                           (contains? (:pixels state) p) 0
                           :else 1))
             $)
        (string/join $)
        (Integer/parseInt $ 2)
        (get enhancement-algorithm $)
        (= \# $)))

(defn tick
  {:test (fn []
           (is= (tick {:pixels #{[0 0]} :background :dark}
                      enhancement-algorithm)
                {:pixels     #{[1 0] [1 1] [0 1]}
                 :background :light}))}
  [state enhancement-algorithm]
  (let [n-background (next-background (:background state) enhancement-algorithm)]
    (-> state
        (update :background next-background enhancement-algorithm)
        (assoc :pixels (->> state
                            (:pixels)
                            (mapcat get-focus-elements)
                            (distinct)
                            (filter (fn [p]
                                      (if (= n-background :light)
                                        (not (light? state enhancement-algorithm p))
                                        (light? state enhancement-algorithm p))))
                            (set))))))

(defn tick-n
  {:test (fn []
           (is= (-> (tick-n (create-initial-state test-puzzle-input)
                            test-enhancement-algorithm
                            2)
                    (:pixels)
                    (count))
                35))}
  [state enhancement-algorithm n]
  (->> (range n)
       (reduce (fn [state _]
                 (tick state enhancement-algorithm))
               state)))

(comment
  (deftest puzzle-part-1
    (is= (time (->> (tick-n (create-initial-state puzzle-input)
                            enhancement-algorithm
                            2)
                    (:pixels)
                    (count)))
         ; "Elapsed time: 133.439875 msecs"
         4964))
  )

(comment
  (deftest puzzle-part-2
    (is= (time (->> (tick-n (create-initial-state puzzle-input)
                            enhancement-algorithm
                            50)
                    (:pixels)
                    (count)))
         ; "Elapsed time: 4858.294375 msecs"
         13202))
  )