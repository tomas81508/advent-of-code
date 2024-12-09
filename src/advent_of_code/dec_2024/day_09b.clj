(ns advent-of-code.dec-2024.day-09b
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/dec_2024/day_09_input.txt"))

(def test-input "2333133121414131402")

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                [[0 2] [nil 3] [1 3] [nil 3] [2 1] [nil 3] [3 3] [nil 1] [4 2] [nil 1] [5 4] [nil 1] [6 4] [nil 1] [7 3] [nil 1] [8 4] [9 2]]))}
  [input]
  (let [r (->> input
               (seq)
               (map (comp read-string str))
               (into [])
               (reduce-kv (fn [a index v]
                            (if (even? index)
                              (reduce conj a (repeat v (/ index 2)))
                              (reduce conj a (repeat v nil))))
                          [])
               (reduce (fn [a v]
                         (cond (= v (:stacking a)) (update a :counter inc)
                               (= (:stacking a) :nothing) (assoc a :stacking v :counter 1)
                               :else (-> a
                                         (update :result conj [(:stacking a) (:counter a)])
                                         (assoc :stacking v :counter 1))))
                       {:result   []
                        :stacking :nothing
                        :counter  0}))]
    (conj (:result r) [(:stacking r) (:counter r)])))


(loop [m [[0 2] [nil 3] [1 3] [nil 3] [2 1] [nil 3] [3 3] [nil 1] [4 2] [nil 1] [5 4] [nil 1] [6 4] [nil 1] [7 3] [nil 1] [8 4] [9 2]]
       index (dec (count m))]
  (let []))