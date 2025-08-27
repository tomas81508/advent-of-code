(ns advent-of-code.dec-2016.day-24
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.set :refer [union difference]]
            [clojure.math.combinatorics :refer [permutations]]))


(def input (->> (slurp "src/advent_of_code/dec_2016/day_24_input.txt")
                (string/split-lines)))

(def test-input ["###########"
                 "#0.1.....2#"
                 "#.#######.#"
                 "#4.......3#"
                 "###########"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:dirt     {0 [1 1]
                            1 [3 1]
                            2 [9 1]
                            3 [9 3]
                            4 [1 3]}
                 :passages #{[1 1] [2 1] [3 1] [4 1] [5 1] [6 1] [7 1] [8 1] [9 1]
                             [1 2] [9 2]
                             [1 3] [2 3] [3 3] [4 3] [5 3] [6 3] [7 3] [8 3] [9 3]}}))}
  [input]
  (->> input
       (reduce-kv (fn [a y row]
                    (->> (vec row)
                         (reduce-kv (fn [a x c]
                                      (case c
                                        \# a
                                        \. (update a :passages conj [x y])
                                        (-> a
                                            (update :dirt assoc (read-string (str c)) [x y])
                                            (update :passages conj [x y]))))
                                    a)))
                  {:dirt     {}
                   :passages #{}})))

(def test-state (create-state test-input))
(def state (create-state input))

(def directions [[-1 0] [1 0] [0 -1] [0 1]])

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step (:passages test-state) [1 1])
                #{[2 1] [1 2]}))}
  [non-visited-nodes from]
  (->> directions
       (map (fn [d] (map + from d)))
       (filter (fn [p] (contains? non-visited-nodes p)))
       (set)))

(defn find-distance-between-coordinates
  {:test (fn []
           (is= (find-distance-between-coordinates test-state [1 1] [1 3]) 2)
           (is= (find-distance-between-coordinates test-state [1 3] [3 1]) 4)
           (is= (find-distance-between-coordinates test-state [3 1] [9 1]) 6)
           (is= (find-distance-between-coordinates test-state [9 1] [9 3]) 2))}
  [state start goal]
  (loop [non-visited-nodes (:passages state)
         current-positions #{start}
         steps 0]
    (if (contains? current-positions goal)
      steps
      (let [new-current-positions (->> current-positions
                                       (map (fn [cp] (walk-a-step non-visited-nodes cp)))
                                       (reduce union))]
        (recur (difference non-visited-nodes new-current-positions)
               new-current-positions
               (inc steps))))))

(defn create-map
  {:test (fn []
           (is= (create-map test-state)
                {0 {1 2 2 8 3 10 4 2}
                 1 {0 2 2 6 3 8 4 4}
                 2 {0 8 1 6 4 10 3 2}
                 3 {0 10 1 8 2 2 4 8}
                 4 {0 2 1 4 2 10 3 8}}))}
  [state]
  (->> (:dirt state)
       (reduce-kv (fn [a k v]
                    (assoc a k (->> (dissoc (:dirt state) k)
                                    (reduce-kv (fn [a n p]
                                                 (assoc a n (find-distance-between-coordinates state v p)))
                                               {}))))
                  {})))

(defn walk
  {:test (fn []
           (is= (walk (create-map test-state) [4 1 2 3])
                14))}
  [distance-map waypoints]
  (->> (cons 0 waypoints)
       (partition 2 1)
       (map (fn [points] (get-in distance-map points)))
       (reduce +)))

(defn find-min-distance
  {:test (fn []
           (is= (find-min-distance test-state)
                14))}
  [state]
  (let [distance-map (create-map state)]
    (->> (permutations (range 1 (count (:dirt state))))
         (map (fn [ps] (walk distance-map ps)))
         (apply min))))

(comment
  (time (find-min-distance state))
  )

(defn find-min-distance2
  {:test (fn []
           (is= (find-min-distance2 test-state)
                14))}
  [state]
  (let [distance-map (create-map state)]
    (->> (permutations (range 1 (count (:dirt state))))
         (map (fn [coll] (conj (vec coll) 0)))
         (map (fn [ps] (walk distance-map ps)))
         (apply min))))

(comment
  (time (find-min-distance2 state))
  (+ (/ (- 1684 (* 3 56)) 4) 56)
  )
