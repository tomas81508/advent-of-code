(ns advent-of-code.dec-2019.day-10
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_10.txt") $
        (clojure.string/split-lines $)))

(defn gcd
  {:test (fn []
           (is= (gcd 71 83) 1)
           (is= (gcd 71 0) 71)
           (is= (gcd 1244 432) 4))}
  [a b]
  {:pre [a b]}
  (if (zero? b) (if (pos? a) a (- a)) (recur b (mod a b))))


(defn create-state
  {:test (fn []
           (is= (create-state ["#.##."
                               "..#.."])
                {:asteroids #{[0 0] [2 0] [3 0] [2 1]}
                 :min-x     0
                 :max-x     3
                 :min-y     0
                 :max-y     1}))}
  [strings]
  (->> strings
       (map-indexed (fn [index line] [index line]))
       (reduce (fn [a [y line]]
                 (->> line
                      (map-indexed (fn [index v] [index v]))
                      (reduce (fn [a [x v]]
                                (if (= v \#)
                                  (-> a
                                      (update :asteroids conj [x y])
                                      (update :min-x min x)
                                      (update :max-x max x)
                                      (update :min-y min y)
                                      (update :max-y max y))
                                  a))
                              a)))
               {:asteroids #{}
                :min-x     0
                :max-x     0
                :min-y     0
                :max-y     0})))

(defn get-asteroid-in-sight
  {:test (fn []
           (is= (-> (create-state ["#.##"])
                    (get-asteroid-in-sight [0 0] [1 0]))
                [2 0])
           (is= (-> (create-state ["#..."])
                    (get-asteroid-in-sight [0 0] [1 0]))
                nil)
           (is= (-> (create-state ["#.##"
                                   ".##."
                                   "..##"])
                    (get-asteroid-in-sight [1 1] [1 1]))
                [2 2])
           (is= (-> (create-state ["#.##"
                                   ".##."
                                   "..##"])
                    (get-asteroid-in-sight [0 0] [2 1]))
                [2 1]))}
  [{asteroids :asteroids min-x :min-x max-x :max-x min-y :min-y max-y :max-y}
   origin direction]
  {:pre [(= (gcd (first direction) (second direction)) 1)]}
  (loop [steps 1]
    (let [maybe-asteroid (map +
                              origin
                              (map (fn [x] (* x steps)) direction))]
      (cond (not (and (<= min-x (first maybe-asteroid) max-x)
                      (<= min-y (second maybe-asteroid) max-y)))
            nil

            (contains? asteroids maybe-asteroid)
            maybe-asteroid

            :else
            (recur (inc steps))))))

(defn get-all-angles
  {:test (fn []
           (is= (-> (create-state [".#..#"
                                   "....."
                                   "#####"
                                   "....#"
                                   "...##"])
                    (get-all-angles [3 4]))
                [[-3 -4] [-3 -2] [-3 -1] [-2 -3] [-2 -1] [-1 -4] [-1 -3] [-1 -2] [-1 -1] [-1 0] [0 -1] [1 -4] [1 -3] [1 -2] [1 -1] [1 0]]))}
  [{min-x :min-x max-x :max-x min-y :min-y max-y :max-y} origin]
  (->> (for [x (range min-x (inc max-x))
             y (range min-y (inc max-y))]
         (let [v (map - [x y] origin)]
           (when (= (apply gcd v) 1)
             v)))
       (remove (fn [x] (or (nil? x) (= x origin))))))

(defn get-asteroids-in-sight
  {:test (fn []
           (is= (-> (create-state [".#..#"
                                   "....."
                                   "#####"
                                   "....#"
                                   "...##"])
                    (get-asteroids-in-sight [3 4]))
                #{[4 3] [2 2] [4 2] [0 2] [4 4] [1 2] [3 2] [4 0]}))}
  [{min-x :min-x max-x :max-x min-y :min-y max-y :max-y :as state} origin]
  (->> (get-all-angles state origin)
       (map (partial get-asteroid-in-sight state origin))
       (remove nil?)
       (set)))

(defn get-best-location
  {:test (fn []
           (is= (-> (create-state [".#..#"
                                   "....."
                                   "#####"
                                   "....#"
                                   "...##"])
                    (get-best-location)
                    (:origin))
                [3 4]))}
  [state]
  (->> (:asteroids state)
       (map (fn [origin]
              {:origin    origin
               :asteroids (get-asteroids-in-sight state origin)}))
       (reduce (fn [a v]
                 (if (> (count (:asteroids v))
                        (count (:asteroids a)))
                   v
                   a)))))

(deftest puzzle-a
         (is= (time (-> (get-puzzle-input)
                        (create-state)
                        (get-best-location)
                        (update :asteroids count)))
              ; "Elapsed time: 563.419634 msecs"
              {:origin [20 18], :asteroids 280}))

(defn sort-all-angles
  {:test (fn []
           (is= (sort-all-angles [[-2 -1] [-1 -2] [-1 -1] [0 1] [-1 0] [0 -1] [1 -2] [1 -1] [1 0]])
                [[0 -1] [1 -2] [1 -1] [1 0] [0 1] [-1 0] [-2 -1] [-1 -1] [-1 -2]]))}
  [coordinates]
  (concat (->> coordinates
               (filter (fn [[x y]] (and (zero? x) (neg? y)))))
          (->> coordinates
               (filter (fn [[x _]] (pos? x)))
               (sort-by (fn [[x y]] (/ y x))))
          (->> coordinates
               (filter (fn [[x y]] (and (zero? x) (pos? y)))))
          (->> coordinates
               (filter (fn [[x _]] (neg? x)))
               (sort-by (fn [[x y]] (/ y x))))))

(defn use-laser
  {:test (fn []
           (is= (as-> (create-state [".#....#####...#.."
                                     "##...##.#####..##"
                                     "##...#...#.#####."
                                     "..#.....X...###.."
                                     "..#.#.....#....##"]) $
                      (use-laser $ [8 3])
                      (take 9 $))
                [[8 1] [9 0] [9 1] [10 0] [9 2] [11 1] [12 1] [11 2] [15 1]]))}
  [state origin]
  (let [angles (->> (get-all-angles state origin)
                    (sort-all-angles))]
    (loop [result []
           [angle & angles] angles
           state (update state
                         :asteroids
                         (fn [asteriods]
                           (set (remove #{origin} asteriods))))]
      (if (empty? (:asteroids state))
        result
        (let [asteroid (get-asteroid-in-sight state origin angle)]
          (if asteroid
            (recur (conj result asteroid)
                   (concat angles [angle])
                   (update state
                           :asteroids
                           (fn [asteroids]
                             (set (remove #{asteroid} asteroids)))))
            (recur result
                   angles
                   state)))))))

(deftest puzzle-b
         (is= (time (as-> (get-puzzle-input) $
                          (create-state $)
                          (use-laser $ [20 18])
                          (nth $ 199)))
              ; "Elapsed time: 67.982128 msecs"
              [7 6]))






