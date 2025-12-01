(ns advent-of-code.dec-2025.day-01
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

(def input (->> (slurp "src/advent_of_code/dec_2025/day_01_input.txt")
                (string/split-lines)))

(def test-input ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"])

(defn turn
  {:test (fn []
           (is= (turn 50 "L" 68)
                82))}
  [position direction amount]
  (rem (+ 100 ((if (= direction "L") - +) position amount)) 100))

(defn get-password
  {:test (fn []
           (is= (get-password test-input) 3))}
  [input]
  (->> input
       (reduce (fn [a v]
                 (let [[_ direction amount] (re-find #"([L|R])(\d+)" v)
                       new-position (turn (:position a) direction (read-string amount))]
                   {:position new-position
                    :zeroes   (if (zero? new-position) (inc (:zeroes a)) (:zeroes a))}))
               {:position 50 :zeroes 0})
       (:zeroes)))

(comment
  (get-password input)
  )

;; Part 2

(defn turn-2
  {:test (fn []
           (is= (turn-2 50 "L" 68)
                {:position 82 :passed-zero-count 1})
           (is= (turn-2 50 "R" 68)
                {:position 18 :passed-zero-count 1})
           (is= (turn-2 50 "L" 268)
                {:position 82 :passed-zero-count 3})
           (is= (turn-2 50 "R" 1000)
                {:position 50 :passed-zero-count 10})
           (is= (turn-2 50 "L" 1000)
                {:position 50 :passed-zero-count 10})
           (is= (turn-2 50 "L" 50)
                {:position 0 :passed-zero-count 1})
           (is= (turn-2 50 "R" 50)
                {:position 0 :passed-zero-count 1})
           (is= (turn-2 0 "R" 100)
                {:position 0 :passed-zero-count 1})
           (is= (turn-2 0 "L" 100)
                {:position 0 :passed-zero-count 1})
           (is= (turn-2 0 "L" 5)
                {:position 95 :passed-zero-count 0})
           (is= (turn-2 0 "R" 5)
                {:position 5 :passed-zero-count 0})
           (is= (turn-2 50 "L" 18)
                {:position 32 :passed-zero-count 0}))}
  [position direction amount]
  (let [direction-operator (if (= direction "L") dec inc)]
    (loop [zeroes 0
           position position
           amount amount]
      (if (zero? amount)
        {:position position :passed-zero-count zeroes}
        (let [position (as-> (direction-operator position) $
                             (cond (neg? $) 99
                                   (> $ 99) 0
                                   :else $))]
          (recur (if (zero? position) (inc zeroes) zeroes)
                 position
                 (dec amount)))))))

(defn get-password-2
  {:test (fn []
           (is= (get-password-2 test-input) 6))}
  [input]
  (->> input
       (reduce (fn [a v]
                 (let [[_ direction amount] (re-find #"([L|R])(\d+)" v)
                       {new-position      :position
                        passed-zero-count :passed-zero-count} (turn-2 (:position a) direction (read-string amount))]
                   (-> a
                       (assoc :position new-position)
                       (update :zeroes + passed-zero-count))))
               {:position 50 :zeroes 0})
       (:zeroes)))

(comment
  (get-password-2 input)
  )