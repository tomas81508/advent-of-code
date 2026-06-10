(ns advent-of-code.dec-2019.day-21
  (:require [advent-of-code.dec-2019.day-09 :refer [run]]
            [clojure.string]
            [clojure.edn :as edn]))

; with Daniel Gullberg

(def puzzle-input (as-> (slurp "src/advent_of_code/dec_2019/day_21.txt") $
                        (clojure.string/split $ #",")
                        (map edn/read-string $)
                        (into [] $)))

(defn get-output-string
  [output]
  (->> output
       (map char)
       (apply str)))

(defn print-output!
  [output]
  (-> (get-output-string output)
      (println)))

(defn run-with-input
  [& instructions]
  (let [input (->> instructions
                   (map (fn [instruction] (concat (map int instruction) [10])))
                   (flatten))]
    (println input)
    (run puzzle-input input)))

(def puzzle-1 ["NOT A J" ; J if hole at a
               "NOT B T" ; T if hole at b
               "OR T J"  ; J if hole at a or b
               "NOT C T" ; T if hole at c
               "OR T J"  ; J if hole at c or (a or b)
               "AND D J" ; J if ground at D and hole at any of a, b or c
               "WALK"])

(def puzzle-2 ["NOT A J" ; J if hole at a
               "NOT B T" ; T if hole at b
               "OR T J"  ; J if hole at a or b
               "NOT C T" ; T if hole at c
               "OR T J"  ; J if hole at c or (a or b)
               "AND D J" ; J if ground at D and hole at any of a, b or c
               "NOT E T" ; T if hole at E
               "NOT T T" ; T if ground at E
               "OR H T"  ; T if ground at E or H
               "AND T J" ; J if ground at (E or H) and D and hole at any of a, b or c
               "RUN"])

(->> (apply run-with-input puzzle-2)
     (:outputs)
     (print-output!))

