(ns advent-of-code.dec-2019.day-21
  (:require [ysera.test :refer [is is= is-not]]
            [advent-of-code.dec-2019.day-09 :refer [run
                                                    create-program]]))

(def puzzle-input (as-> (slurp "src/advent_of_code/dec_2019/day_21.txt") $
                        (clojure.string/split $ #",")
                        (map read-string $)
                        (into [] $)))

(run puzzle-input)

(int \N)

(defn run-with-input
  [& instructions]
  (let [input (->> instructions
                   (map (fn [instruction] (map int instruction)))
                   (interpose 10)
                   (flatten))]
    (run puzzle-input input)))

(def puzzle-1 ["NOT A J"
               "NOT B T"
               "AND D T"
               "OR T J"
               "NOT C T"
               "OR T J"
               "AND D J"
               "WALK"])

(apply run-with-input puzzle-1)

