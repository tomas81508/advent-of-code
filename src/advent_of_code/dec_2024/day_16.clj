(ns advent-of-code.dec-2024.day-16
  (:require [clojure.string :refer [split split-lines] :as string]
            [advent-of-code.test :refer [is is-not is=]]))

(def input (split-lines (slurp "src/advent_of_code/dec_2024/day_16_input.txt")))

(def test-input-1 ["###############"
                   "#.......#....E#"
                   "#.#.###.#.###.#"
                   "#.....#.#...#.#"
                   "#.###.#####.#.#"
                   "#.#.#.......#.#"
                   "#.#.#####.###.#"
                   "#...........#.#"
                   "###.#.#####.#.#"
                   "#...#.....#.#.#"
                   "#.#.#.###.#.#.#"
                   "#.....#...#.#.#"
                   "#.###.#.#.#.#.#"
                   "#S..#.....#...#"
                   "###############"])

(def test-input-2 ["#################"
                   "#...#...#...#..E#"
                   "#.#.#.#.#.#.#.#.#"
                   "#.#.#.#...#...#.#"
                   "#.#.#.#.###.#.#.#"
                   "#...#.#.#.....#.#"
                   "#.#.#.#.#.#####.#"
                   "#.#...#.#.#.....#"
                   "#.#.#####.#.###.#"
                   "#.#.#.......#...#"
                   "#.#.###.#####.###"
                   "#.#.#...#.....#.#"
                   "#.#.#.#####.###.#"
                   "#.#.#.........#.#"
                   "#.#.#.#########.#"
                   "#S#.............#"
                   "#################"])

(defn get-walls
  [input]
  (->> input
       (reduce-kv (fn [walls y row]
                    (->> (into [] row)
                         (reduce-kv (fn [walls x c]
                                      (if (= c \#) (conj walls [x y]) walls))
                                    walls)))
                  #{})))

(def test-walls-1 (get-walls test-input-1))
(def test-walls-2 (get-walls test-input-2))
(def walls (get-walls input))

(defn create-initial-state
  [input]
  (let [start-position (->> input
                            (map-indexed (fn [y row] [y row]))
                            (some (fn [[y row]]
                                    (->> row
                                         (map-indexed (fn [x c] [x c]))
                                         (some (fn [[x c]] (when (= c \S) [x y])))))))]
    {:visited  {start-position {[1 0] 0}}
     :boundary [{:p start-position :d [1 0] :cost 0}]}))

(def state (create-initial-state input))
(def test-state-1 (create-initial-state test-input-1))
(def test-state-2 (create-initial-state test-input-2))

(def turn-left {[1 0] [0 -1], [0 -1] [-1 0], [-1 0] [0 1], [0 1] [1 0]})
(def turn-right {[1 0] [0 1], [0 1] [-1 0], [-1 0] [0 -1], [0 -1] [1 0]})

(defn get-directions
  {:test (fn []
           (is= (into #{} (get-directions test-walls-1 [1 13] [1 0]))
                #{[1 0] [0 -1]}))}
  [walls p d]
  (let [naive-directions [d (turn-left d) (turn-right d)]]
    (->> naive-directions
         (remove (fn [d] (contains? walls (map + p d)))))))

(defn get-next-positions
  {:test (fn []
           (is= (->> (get-next-positions test-walls-1 [1 13] [1 0])
                     (into #{}))
                #{{:p [2 13] :d [1 0] :cost 1}
                  {:p [1 13] :d [0 -1] :cost 1000}})
           (is= (->> (get-next-positions test-walls-2 [1 15] [1 0])
                     (into #{}))
                #{{:p [1 15] :d [0 -1] :cost 1000}}))}
  [walls p d]
  (let [directions (get-directions walls p d)]
    (->> directions
         (map (fn [nd] {:p (if (= nd d) (map + nd p) p) :d nd :cost (if (= nd d) 1 1000)})))))

(defn deja-vu?
  [visited {p :p d :d cost :cost}]
  )

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-walls-1 test-state-1)
                {:visited  {[1 13] {[1 0] 0 [0 -1] 1000}
                            [2 13] {[1 0] 1}}
                 :boundary [{:p [2 13] :d [1 0] :cost 1}
                            {:p [1 13] :d [0 -1] :cost 1000}]})
           (is= (->> test-state-1
                     (walk-a-step test-walls-1)
                     (walk-a-step test-walls-1))
                {:visited {[1 13] {[1 0] 0, [0 -1] 1000}
                           [2 13] {[1 0] 1}
                           [3 13] {[1 0] 2}
                           [1 12] {[0 -1] 1001}}
                 :boundary [{:p [3 13], :d [1 0], :cost 2}
                            {:p [1 12], :d [0 -1], :cost 1001}]}))}
  [walls state]
  (let [visited (:visited state)
        boundary (:boundary state)
        new-boundary (->> boundary
                          (map (fn [{p :p d :d cost :cost}]
                                 (let [next-positions (get-next-positions walls p d)]
                                   (->> next-positions
                                        (map (fn [np] (update np :cost + cost)))
                                        (remove (fn [np] (deja-vu? visited np)))))))
                          (flatten))]
    (-> state
        (update :visited (fn [visited] (reduce (fn [visited {p :p d :d cost :cost}]
                                                 (update-in visited
                                                            [p d]
                                                            (fn [old-cost]
                                                              (if old-cost
                                                                (min old-cost cost)
                                                                cost))))
                                               visited
                                               new-boundary)))
        (assoc :boundary new-boundary))))


















