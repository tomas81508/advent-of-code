(ns advent-of-code.dec-2021.day-05
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def test-input "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2")

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_05_input.txt"))

(defn only-horizontal-and-vertical
  [[x1 y1 x2 y2]]
  (or (= x1 x2) (= y1 y2)))

(defn create-state
  {:test (fn []
           (is= (create-state "0,9 -> 5,9\n8,0 -> 0,8" only-horizontal-and-vertical)
                {:clouds {[0 9] 1 [1 9] 1 [2 9] 1 [3 9] 1 [4 9] 1 [5 9] 1}})
           (let [state (create-state test-input only-horizontal-and-vertical)]
             (is-not (get-in state [:clouds [0 0]]))
             (is= (get-in state [:clouds [3 4]]) 2)))}
  [input filter-pred-fn]
  {:clouds (->> input
                (clojure.string/split-lines)
                (map (fn [line]
                       (let [pattern (re-pattern "(\\d+),(\\d+) -> (\\d+),(\\d+)")
                             [_ x1 y1 x2 y2] (re-matches pattern line)]
                         (map read-string [x1 y1 x2 y2]))))
                (filter filter-pred-fn)
                (reduce (fn [a [x1 y1 x2 y2]]
                          (let [direction-x (cond (pos? (- x2 x1)) 1
                                                  (neg? (- x2 x1)) -1
                                                  :else 0)
                                direction-y (cond (pos? (- y2 y1)) 1
                                                  (neg? (- y2 y1)) -1
                                                  :else 0)]
                            (->> (loop [cells []
                                        x x1
                                        y y1]
                                   (if (and (= x x2) (= y y2))
                                     (conj cells [x y])
                                     (recur (conj cells [x y])
                                            (+ x direction-x)
                                            (+ y direction-y))))
                                 (reduce (fn [a c]
                                           (update a c (fn [v] (if v (inc v) 1))))
                                         a))))
                        {})
                )})

(defn number-of-overlaps
  {:test (fn []
           (is= (-> (create-state test-input only-horizontal-and-vertical)
                    (number-of-overlaps))
                5))}
  [state]
  (->> (:clouds state)
       (vals)
       (filter (fn [v] (>= v 2)))
       (count)))

(deftest puzzle-a
         (is= (time (-> puzzle-input
                        (create-state only-horizontal-and-vertical)
                        (number-of-overlaps)))
              ; "Elapsed time: 79.946437 msecs"
              7142)
         )

(defn visualize-board
  {:test (fn []
           (is= (visualize-board (create-state test-input (constantly true)))
                (str "1.1....11.\n"
                     ".111...2..\n"
                     "..2.1.111.\n"
                     "...1.2.2..\n"
                     ".112313211\n"
                     "...1.2....\n"
                     "..1...1...\n"
                     ".1.....1..\n"
                     "1.......1.\n"
                     "222111....")))}
  [state]
  (let [max-x (->> (:clouds state)
                   (keys)
                   (map first)
                   (reduce max))
        max-y (->> (:clouds state)
                   (keys)
                   (map second)
                   (reduce max))]
    (->> (for [y (range (inc max-y))
               x (range (inc max-x))]
           [x y])
         (partition (inc max-x))
         (map (fn [row]
                (->> (map (fn [c] (if-let [value (get-in state [:clouds c])]
                                    (str value)
                                    "."))
                          row)
                     (clojure.string/join ""))))
         (clojure.string/join "\n")))
  )

(deftest test-input-b
         (is= (-> test-input
                  (create-state (constantly true))
                  (number-of-overlaps))
              12))

(deftest puzzle-b
         (is= (time (-> puzzle-input
                        (create-state (constantly true))
                        (number-of-overlaps)))
              ; "Elapsed time: 136.253558 msecs"
              20012))
