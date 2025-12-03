(ns advent-of-code.dec-2025.day-03
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

; together with Daniel Gullberg

(def input (->> (slurp "src/advent_of_code/dec_2025/day_03_input.txt")
                (string/split-lines)))

(def test-input ["987654321111111"
                 "811111111111119"
                 "234234234234278"
                 "818181911112111"])

(defn create-number
  [digits]
  (read-string (apply str digits)))

(defn get-largest-joltage
  {:test (fn []
           (is= (get-largest-joltage "234234234234278" 2) 78)
           (is= (get-largest-joltage "818181911112111" 12) 888911112111))}
  [line number-of-batteries]
  (let [numbers (map (comp read-string str) (seq line))]
    (loop [[d & ds] (drop number-of-batteries numbers)
           digits (into [] (take number-of-batteries numbers))]
      (if (nil? d)
        (create-number digits)
        (recur ds (loop [index 0]
                    (if (= (inc index) (count digits))
                      (if (< (nth digits index) d)
                        (assoc digits index d)
                        digits)
                      (if (< (nth digits index) (nth digits (inc index)))
                        (into [] (concat (take index digits) (drop (inc index) digits) [d]))
                        (recur (inc index))))))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 357))}
  [input]
  (->> input
       (reduce (fn [a line]
                 (+ a (get-largest-joltage line 2)))
               0)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 3121910778619))}
  [input]
  (->> input
       (reduce (fn [a line]
                 (+ a (get-largest-joltage line 12)))
               0)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 20.012 msecs"
  ;=> 17405

  (time (part-2 input))
  ;; "Elapsed time: 61.051417 msecs"
  ;=> 171990312704598
  )




