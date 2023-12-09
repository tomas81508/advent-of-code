(ns advent-of-code.dec-2023.day-04
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not]]))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_04_input.txt")
                (clojure.string/split-lines)))

(def test-input ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
                 "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
                 "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
                 "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
                 "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
                 "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"])

(defn calculate-score
  {:test (fn []
           (is= (calculate-score "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
                4))}
  [l]
  (let [[_ wns ns] (re-find #"Card[ ]+\d+: ([\d ]+) \| ([\d ]+)" l)
        winning-numbers (->> (clojure.string/split wns #"[^\d]")
                             (keep (fn [x]
                                     (when (not= x "")
                                       (read-string x))))
                             (into #{}))
        numbers (->> (clojure.string/split ns #"[^\d]")
                     (keep (fn [x]
                             (when (not= x "")
                               (read-string x))))
                     (into #{}))]
    (count (clojure.set/intersection winning-numbers numbers))))

(defn calculate-worth
  {:test (fn []
           (is= (calculate-worth "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
                8))}
  [l]
  (let [score (calculate-score l)]
    (if (zero? score)
      0
      (apply * (repeat (dec score) 2)))))

(deftest test-puzzle-a
  (is= (->> test-input
            (map calculate-worth)
            (reduce +))
       13))

(deftest puzzle-a
  (is= (time (->> input
                  (map calculate-worth)
                  (reduce +)))
       ; "Elapsed time: 6.95966 msecs"
       23678))

;; Part 2

(defn score-with-new-rules
  {:test (fn []
           (is= (score-with-new-rules test-input)
                30))}
  [input]
  (->> input
       (map-indexed (fn [i r] [i r]))
       (reduce (fn [a [i r]]
                 (let [score (calculate-score r)
                       number-of-boards (get-in a [:boards i :copies])]
                   (-> a
                       (update :instances + (* score number-of-boards))
                       (update :boards (fn [boards]
                                         (->> (range (inc i) (+ i (inc score)))
                                              (reduce (fn [boards i]
                                                        (update-in boards [i :copies] + number-of-boards))
                                                      boards)))))))
               {:boards    (into [] (repeat (count input) {:copies 1}))
                :instances (count input)})
       (:instances)))

(deftest puzzle-b
  (is= (time (score-with-new-rules input))
       ; "Elapsed time: 7.582723 msecs"
       15455663))