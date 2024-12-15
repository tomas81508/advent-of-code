(ns advent-of-code.dec-2024.day-15
  (:require [clojure.string :refer [split]]
            [advent-of-code.test :refer [is=]]))

; with Mattias Liljeström and Daniel Gullberg

(def input (split (slurp "src/advent_of_code/dec_2024/day_15_input.txt") #"\n\n"))
(def test-input (split (slurp "src/advent_of_code/dec_2024/day_15_test_input.txt") #"\n\n"))

(defn parse-input [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn all-of-type [ch the-map]
  (into #{} (keys (filter (fn [[_ v]] (= v ch)) the-map))))

(defn create-warehouse [[input moves]]
  (let [warehouse (parse-input (split input #"\n"))]
    {:robot (first (all-of-type \@ warehouse))
     :walls (all-of-type \# warehouse)
     :boxes (all-of-type \O warehouse)
     :moves (clojure.string/replace moves #"\n" "")}))

(def moves {\^ [0 -1] \> [1 0] \v [0 1] \< [-1 0]})

(def warehouse (create-warehouse input))
(def test-warehouse (create-warehouse test-input))

(defn get-movable-boxes
  {:test (fn []
           (is= (get-movable-boxes test-warehouse [-1 0] [4 4])
                {[3 4] [2 4]})
           (is= (get-movable-boxes test-warehouse [-1 0] [1 1])
                :wall))}
  [warehouse dir pos]
  (loop [moveable-boxes {}
         pos pos]
    (let [new-pos (mapv + dir pos)]
      (cond (get-in warehouse [:walls new-pos]) :wall
            (get-in warehouse [:boxes new-pos]) (recur (assoc moveable-boxes new-pos (mapv + dir new-pos)) new-pos)
            :else moveable-boxes))))

(defn move [warehouse]
  (->> (:moves warehouse)
       (reduce (fn [warehouse move]
                 (let [dir (moves move)
                       pos (warehouse :robot)
                       movable-boxes (get-movable-boxes warehouse dir pos)]
                   (if (= movable-boxes :wall)
                     warehouse
                     (-> warehouse
                         (assoc :robot (mapv + dir pos))
                         (update :boxes (fn [boxes] (apply disj boxes (keys movable-boxes))))
                         (update :boxes (fn [boxes] (apply conj boxes (vals movable-boxes))))))))
               warehouse)))

(defn part1 [warehouse]
  (->> warehouse
       (move)
       (:boxes)
       (map (fn [[x y]] (+ x (* 100 y))))
       (reduce +)))

(comment
  (part1 warehouse)
  )