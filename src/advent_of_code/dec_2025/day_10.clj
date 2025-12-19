(ns advent-of-code.dec-2025.day-10
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

; together with Daniel Gullberg and Mattias Liljeström

(def input (->> (slurp "src/advent_of_code/dec_2025/day_10_input.txt")
                (string/split-lines)))

(def test-input ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
                 "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
                 "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"])

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input)
                [{:indicators-goal [0 1 1 0] :buttons [[3] [1 3] [2] [2 3] [0 2] [0 1]] :joltage-levels [3 5 4 7]}
                 {:indicators-goal [0 0 0 1 0] :buttons [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] :joltage-levels [7 5 12 7 2]}
                 {:indicators-goal [0 1 1 1 0 1] :buttons [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] :joltage-levels [10 11 11 5 10 5]}]))}
  [input]
  (->> input
       (map (fn [row]
              (let [indicators-string (second (re-find #"\[([.#]+)\]" row))
                    button-strings (re-seq #"\([0-9,]+\)" row)
                    joltage-levels-string (->> (re-find #"\{[\d,]+\}" row)
                                               (re-seq #"\d+"))]
                {:indicators-goal (mapv (fn [c] (if (= c \#) 1 0)) indicators-string)
                 :buttons         (->> button-strings
                                       (mapv (fn [bs] (->> (re-seq #"\d+" bs)
                                                           (mapv read-string)))))
                 :joltage-levels  (mapv read-string joltage-levels-string)})))))

(def parsed-test-input (parse-input test-input))
(def parsed-input (parse-input input))

(defn button-press-to-geet-indicator-goal
  {:test (fn []
           (is= (button-press-to-geet-indicator-goal {:indicators-goal [0 1 1 0] :buttons [[3] [1 3] [2] [2 3] [0 2] [0 1]] :joltage-levels [3 5 4 7]})
                [[[0 2] [0 1]]
                 [[1 3] [2 3]]
                 [[3] [2] [2 3] [0 2] [0 1]]
                 [[3] [1 3] [2]]])
           (is= (button-press-to-geet-indicator-goal {:indicators-goal [0 0 0 1 0] :buttons [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] :joltage-levels [7 5 12 7 2]})
                [[[0 4] [0 1 2] [1 2 3 4]]
                 [[0 2 3 4] [2 3] [0 1 2] [1 2 3 4]]])
           (is= (button-press-to-geet-indicator-goal {:indicators-goal [0 1 1 1 0 1] :buttons [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] :joltage-levels [10 11 11 5 10 5]})
                [[[0 3 4] [0 1 2 4 5]]
                 [[0 1 2 3 4] [0 1 2 4 5] [1 2]]]))}
  [machine]
  (->> (:buttons machine)
       (reduce (fn [states button]
                 (->> states
                      (mapcat (fn [state]
                                [state
                                 [(->> button
                                       (reduce (fn [a i]
                                                 (update a i (fn [v] (if (zero? v) 1 0))))
                                               (first state)))
                                  (conj (second state) button)]]))))
               [[(vec (repeat (count (:indicators-goal machine)) 0)) []]])
       (filter (fn [state] (= (first state) (:indicators-goal machine))))
       (map (fn [[_ buttons]] buttons))))

(defn count-button-press-to-get-indicator-goal
  {:test (fn []
           (is= (count-button-press-to-get-indicator-goal {:indicators-goal [0 1 1 0] :buttons [[3] [1 3] [2] [2 3] [0 2] [0 1]] :joltage-levels [3 5 4 7]})
                2)
           (is= (count-button-press-to-get-indicator-goal {:indicators-goal [0 0 0 1 0] :buttons [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] :joltage-levels [7 5 12 7 2]})
                3)
           (is= (count-button-press-to-get-indicator-goal {:indicators-goal [0 1 1 1 0 1] :buttons [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] :joltage-levels [10 11 11 5 10 5]})
                2))}
  [machine]
  (->> (button-press-to-geet-indicator-goal machine)
       (map count)
       (apply min)))

(defn solve-1
  {:test (fn []
           (is= (solve-1 parsed-test-input) 7))}
  [input]
  (->> input
       (map count-button-press-to-get-indicator-goal)
       (reduce +)))

(comment
  (solve-1 parsed-input)
  )

(declare count-button-press-to-get-joltage-levels)
(defn count-button-press-to-get-joltage-levels-raw
  {:test (fn []
           (is= (count-button-press-to-get-joltage-levels [[3] [1 3] [2] [2 3] [0 2] [0 1]]
                                                          [3 5 4 7])
                10)
           (is= (count-button-press-to-get-joltage-levels [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]]
                                                          [7 5 12 7 2])
                12)
           (is= (count-button-press-to-get-joltage-levels [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]]
                                                          [10 11 11 5 10 5])
                11))}
  [buttons goal]
  (cond (every? zero? goal) 0

        (some neg? goal) 1000000

        :else
        (let [even-odd-lights (->> goal (mapv (fn [v] (rem v 2))))
              pressed-buttons (button-press-to-geet-indicator-goal {:buttons         buttons
                                                                    :indicators-goal even-odd-lights})]
          (if (empty? pressed-buttons)
            1000000
            (->> pressed-buttons
                 (map (fn [pbs]
                        (let [pressed (count pbs)
                              new-goal (->> (flatten pbs)
                                            (frequencies)
                                            (reduce-kv (fn [goal index n] (update goal index - n)) goal))]
                          (+ pressed
                             (* 2
                                (count-button-press-to-get-joltage-levels buttons
                                                                          (mapv (fn [x] (/ x 2)) new-goal)))))))
                 (apply min))))))

(def count-button-press-to-get-joltage-levels (memoize count-button-press-to-get-joltage-levels-raw))

(defn solve-2
  {:test (fn []
           (is= (solve-2 parsed-test-input) 33))}
  [input]
  (->> input
       (map (fn [machine] (count-button-press-to-get-joltage-levels (:buttons machine) (:joltage-levels machine))))
       (reduce +)))

(comment
  (time (solve-2 parsed-input))
  )
