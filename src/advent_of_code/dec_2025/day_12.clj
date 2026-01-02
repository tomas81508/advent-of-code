(ns advent-of-code.dec-2025.day-12
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.set :as clojure-set]))

(def input (slurp "src/advent_of_code/dec_2025/day_12_input.txt"))

(def test-input "0:\n###\n##.\n##.\n\n1:\n###\n##.\n.##\n\n2:\n.##\n###\n##.\n\n3:\n##.\n###\n##.\n\n4:\n###\n#..\n###\n\n5:\n###\n.#.\n###\n\n4x4: 0 0 0 0 2 0\n12x5: 1 0 1 0 2 2\n12x5: 1 0 1 0 3 2")

(def test-shapes
  (->> test-input
       (string/split-lines)
       (take-while (fn [r] (not (re-find #"^\d+x\d+" r))))
       (partition 4 5)
       (map rest)
       (reduce (fn [a shape]
                 (conj a ))
               [])))

