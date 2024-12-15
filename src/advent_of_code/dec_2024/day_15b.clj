(ns advent-of-code.dec-2024.day-15b
  (:require [clojure.string :refer [split split-lines] :as string]
            [advent-of-code.test :refer [is is-not is=]]))

; with Mattias Liljeström and Daniel Gullberg

(def input (split (slurp "src/advent_of_code/dec_2024/day_15_input.txt") #"\n\n"))
(def test-input (split (slurp "src/advent_of_code/dec_2024/day_15_test_input.txt") #"\n\n"))

(defn parse-atlas-input [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(def test-atlas-input (parse-atlas-input (split-lines (first test-input))))
(def atlas-input (parse-atlas-input (split-lines (first input))))

(def parse-moves {\^ [0 -1] \> [1 0] \v [0 1] \< [-1 0]})

(def moves (->> (string/replace (second input) #"\n" "")
                (map parse-moves)))
(def test-moves (->> (string/replace (second test-input) #"\n" "")
                     (map parse-moves)))

(defn all-of-type [ch atlas]
  (into #{} (keys (filter (fn [[_ v]] (= v ch)) atlas))))

(defn widen [[x y]] [(* 2 x) y])

(defn create-warehouse
  [atlas-input moves]
  {:robot (widen (first (all-of-type \@ atlas-input)))
   :walls (->> (all-of-type \# atlas-input)
               (map (fn [p] (let [wp (widen p)] [wp (update wp 0 inc)])))
               (apply concat)
               (into #{}))
   :boxes (->> (all-of-type \O atlas-input)
               (map (fn [p] (let [wp (widen p)] [wp (update wp 0 inc)])))
               (into #{}))
   :moves moves})

(def warehouse (create-warehouse atlas-input moves))
(def test-warehouse (create-warehouse test-atlas-input test-moves))

(defn wall?
  {:test (fn []
           (is (wall? test-warehouse [0 1]))
           (is (wall? test-warehouse [1 1]))
           (is-not (wall? test-warehouse [2 1])))}
  [warehouse position]
  (get-in warehouse [:walls position]))

(defn get-box
  {:test (fn []
           (is= (get-box test-warehouse [2 5]) [[2 5] [3 5]])
           (is= (get-box test-warehouse [3 5]) [[2 5] [3 5]])
           (is-not (get-box test-warehouse [4 5])))}
  [warehouse position]
  (or (get-in warehouse [:boxes [position (update position 0 inc)]])
      (get-in warehouse [:boxes [(update position 0 dec) position]])))

(defn move-box
  {:test (fn []
           (is= (move-box [[6 4] [7 4]] [-1 0]) [[5 4] [6 4]])
           (is= (move-box [[6 4] [7 4]] [0 -1]) [[6 3] [7 3]]))}
  [box dir]
  (->> box
       (mapv (fn [p] (mapv + p dir)))))

(defn get-movable-boxes
  {:test (fn []
           (is= (get-movable-boxes test-warehouse [-1 0] [8 4])
                {[[6 4] [7 4]] [[5 4] [6 4]]})
           (is= (get-movable-boxes test-warehouse [0 -1] [4 4])
                {[[4 3] [5 3]] [[4 2] [5 2]]})
           (is= (get-movable-boxes test-warehouse [0 -1] [5 4])
                {[[4 3] [5 3]] [[4 2] [5 2]]})
           (is= (get-movable-boxes {:boxes #{[[4 4] [5 4]] [[6 4] [7 4]] [[5 5] [6 5]]}} [0 -1] [5 6])
                {[[4 4] [5 4]] [[4 3] [5 3]] [[6 4] [7 4]] [[6 3] [7 3]] [[5 5] [6 5]] [[5 4] [6 4]]})
           (is= (get-movable-boxes test-warehouse [-1 0] [2 1])
                :wall))}
  [warehouse dir pos]
  (let [x-move (zero? (second dir))]
    (loop [moveable-boxes {}
           pos pos]
      (let [new-pos (mapv + dir pos)
            box (get-box warehouse new-pos)]
        (cond (wall? warehouse new-pos) :wall

              (and box x-move)
              (recur (assoc moveable-boxes box (move-box box dir)) (mapv + dir new-pos))

              box
              (let [left-boxes (get-movable-boxes warehouse dir (first box))
                    right-boxes (get-movable-boxes warehouse dir (second box))]
                (if (or (= :wall left-boxes) (= :wall right-boxes))
                  :wall
                  (merge (assoc moveable-boxes box (move-box box dir)) left-boxes right-boxes)))

              :else moveable-boxes)))))

(defn move [warehouse]
  (->> (:moves warehouse)
       (reduce (fn [warehouse dir]
                 (let [pos (warehouse :robot)
                       movable-boxes (get-movable-boxes warehouse dir pos)]
                   (if (= movable-boxes :wall)
                     warehouse
                     (-> warehouse
                         (assoc :robot (mapv + dir pos))
                         (update :boxes (fn [boxes] (apply disj boxes (keys movable-boxes))))
                         (update :boxes (fn [boxes] (apply conj boxes (vals movable-boxes))))))))
               warehouse)))

(defn part2 [warehouse]
  (->> warehouse
       (move)
       (:boxes)
       (map (fn [[[x y] _]] (+ x (* 100 y))))
       (reduce +)))

(comment
  (part2 test-warehouse)
  (part2 warehouse)
  )