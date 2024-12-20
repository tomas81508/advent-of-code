(ns advent-of-code.dec-2015.day-03
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input []
  (slurp "src/advent_of_code/dec_2015/day_03_input.txt"))

(defn houses-getting-presents
  {:test (fn []
           (is= (houses-getting-presents ">")
                {[0 0] 1
                 [1 0] 1})
           (is= (houses-getting-presents "^>v<")
                {[0 0] 2
                 [0 1] 1
                 [1 1] 1
                 [1 0] 1})
           (is= (houses-getting-presents "^v^v^v^v^v")
                {[0 0] 6
                 [0 1] 5}))}
  [directions]
  (->> directions
       (map (fn [d]
              (case d
                \^ [0 1]
                \> [1 0]
                \v [0 -1]
                \< [-1 0])))
       (reduce (fn [a d]
                 (let [position (map + (:position a) d)]
                   (-> a
                       (update-in [:houses position] (fn [n] (if n (inc n) 1)))
                       (assoc :position position))))
               {:houses   {[0 0] 1}
                :position [0 0]})
       (:houses)))

(deftest puzzle-a
  (is= (->> (get-puzzle-input)
            (houses-getting-presents)
            (keys)
            (count))
       2081))

(defn houses-getting-presents-with-robo-santa
  {:test (fn []
           (is= (houses-getting-presents-with-robo-santa "^v")
                {[0 0]  2
                 [0 1]  1
                 [0 -1] 1})
           (is= (houses-getting-presents-with-robo-santa "^>v<")
                {[0 0] 4
                 [0 1] 1
                 [1 0] 1})
           (is= (houses-getting-presents-with-robo-santa "^v^v^v^v^v")
                {[0 0]  2
                 [0 1]  1 [0 2] 1 [0 3] 1 [0 4] 1 [0 5] 1
                 [0 -1] 1 [0 -2] 1 [0 -3] 1 [0 -4] 1 [0 -5] 1}))}
  [directions]
  (->> directions
       (map (fn [d]
              (case d
                \^ [0 1]
                \> [1 0]
                \v [0 -1]
                \< [-1 0])))
       (partition 2)
       (reduce (fn [a [d-santa d-robo-santa]]
                 (let [santa-position (map + (:santa-position a) d-santa)
                       robo-santa-position (map + (:robo-santa-position a) d-robo-santa)]
                   (-> a
                       (update-in [:houses santa-position] (fn [n] (if n (inc n) 1)))
                       (assoc :santa-position santa-position)
                       (update-in [:houses robo-santa-position] (fn [n] (if n (inc n) 1)))
                       (assoc :robo-santa-position robo-santa-position))))
               {:houses              {[0 0] 2}
                :santa-position      [0 0]
                :robo-santa-position [0 0]})
       (:houses)))

(deftest puzzle-a
  (is= (->> (get-puzzle-input)
            (houses-getting-presents-with-robo-santa)
            (keys)
            (count))
       2341))




