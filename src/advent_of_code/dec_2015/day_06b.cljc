(ns advent-of-code.dec-2015.day-06b
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :refer [split-lines starts-with?]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2015/day_06_input.txt")
       (split-lines)))


(defn turn-on
  [lights x1 y1 x2 y2]
  (->> (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y])
       (reduce (fn [lights k]
                 (update lights k (fn [x] (if x (inc x) 1))))
               lights)))

(defn turn-off
  [lights x1 y1 x2 y2]
  (->> (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y])
       (reduce (fn [lights k]
                 (update lights k (fn [x] (if x (max 0 (dec x)) 0))))
               lights)))

(defn toggle
  [lights x1 y1 x2 y2]
  (->> (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y])
       (reduce (fn [lights k]
                 (update lights k (fn [x] (if x (+ x 2) 2))))
               lights)))

(def args-pattern (re-pattern "[\\w| ]+ (\\d+),(\\d+) through (\\d+),(\\d+)"))

(defn handle-instruction
  [lights instruction]
  (let [args (->> (re-matches args-pattern instruction)
                  (drop 1)
                  (map read-string))]
    (cond (starts-with? instruction "turn off")
          (apply turn-off lights args)

          (starts-with? instruction "turn on")
          (apply turn-on lights args)

          (starts-with? instruction "toggle")
          (apply toggle lights args))))

(deftest puzzle-b
  (is= (->> (get-puzzle-input)
            (reduce handle-instruction {})
            (vals)
            (reduce +))
       15343601))


