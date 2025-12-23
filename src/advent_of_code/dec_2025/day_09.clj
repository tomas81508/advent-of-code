(ns advent-of-code.dec-2025.day-09
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

; together with Daniel Gullberg

(def input (->> (slurp "src/advent_of_code/dec_2025/day_09_input.txt")
                (string/split-lines)))

(def test-input ["7,1" "11,1" "11,7" "9,7" "9,5" "2,5" "2,3" "7,3"])

(defn parse-input [input]
  (mapv (fn [r] (mapv read-string (re-seq #"\d+" r))) input))

(def parsed-test-input (parse-input test-input))
(def parsed-input (parse-input input))

(defn naive-calculation-of-largest-area
  {:test (fn []
           (is= (naive-calculation-of-largest-area parsed-test-input)
                50))}
  [coordinates]
  (loop [[coordinate & coordinates] coordinates
         result 0]
    (if (empty? coordinates)
      result
      (recur coordinates
             (reduce (fn [result c]
                       (max result
                            (->> (map - coordinate c)
                                 (map abs)
                                 (map inc)
                                 (apply *))))
                     result
                     coordinates)))))

(comment
  (time (naive-calculation-of-largest-area parsed-input))
  )


