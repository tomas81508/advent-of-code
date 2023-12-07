(ns advent-of-code.dec-2023.day-07
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not is]]
            [clojure.string :refer [split split-lines]]
            [clojure.math]))

; Together with Mattias Liljeström Daniel Gullberg

(def input (-> (slurp "src/advent_of_code/dec_2023/day_07_input.txt")
               (split-lines)))

(def test-input ["32T3K 765"
                 "T55J5 684"
                 "KK677 28"
                 "KTJJT 220"
                 "QQQJA 483"])


(defn get-score
  {:test (fn []
           (is= (get-score "23456") 0)
           (is= (get-score "32T3K") 1)
           (is= (get-score "KK677") 2)
           (is= (get-score "KTJJJ") 3)
           (is= (get-score "23332") 4)
           (is= (get-score "T5555") 5)
           (is= (get-score "QQQQQ") 6))}
  [hand]
  (let [l (->> (frequencies hand)
               (vals)
               (sort)
               (reverse))]
    (cond (= (first l) 5) 6
          (= (first l) 4) 5
          (= [(first l) (second l)] [3 2]) 4
          (= (first l) 3) 3
          (= [(first l) (second l)] [2 2]) 2
          (= (first l) 2) 1
          :else 0)))

(def card->value (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range 2 15)))

(defn compare-hands
  {:test (fn []
           (is (neg? (compare-hands "32T3K" "T55J5")))
           (is (pos? (compare-hands "KK677" "KTJJT"))))}
  [hand1 hand2]
  (let [score1 (get-score hand1)
        score2 (get-score hand2)]
    (if (not= score1 score2)
      (- score1 score2)
      (loop [[h1 & t1] hand1
             [h2 & t2] hand2]
        (let [v1 (card->value h1)
              v2 (card->value h2)]
          (if (= v1 v2)
            (recur t1 t2)
            (- v1 v2)))))))

(defn total-winnings
  {:test (fn [] (is= (total-winnings test-input) 6440))}
  [input]
  (->> input
       (map (fn [r] (split r #" ")))
       (map (fn [[hand bid-as-sting]] {:hand hand :bid (read-string bid-as-sting)}))
       (sort-by :hand compare-hands)
       (map-indexed (fn [i {bid :bid}] (* (inc i) bid)))
       (reduce +)))

(deftest puzzle-a
  (is= (time (total-winnings input))
       ; "Elapsed time: 111.111403 msecs"
       253638586))


(defn get-score-2
  {:test (fn []
           (is= (get-score-2 "KTJJT") 5)
           (is= (get-score-2 "T55J5") 5)
           (is= (get-score-2 "QQQJA") 5))}
  [hand]
  (if (= hand "JJJJJ")
    6
    (let [f (frequencies hand)
          number-of-jokers (get f \J 0)
          l (as-> (dissoc f \J) $
                  (vals $)
                  (sort $)
                  (reverse $)
                  (into [] $)
                  (update $ 0 + number-of-jokers))]
      (cond (= (first l) 5) 6
            (= (first l) 4) 5
            (= [(first l) (second l)] [3 2]) 4
            (= (first l) 3) 3
            (= [(first l) (second l)] [2 2]) 2
            (= (first l) 2) 1
            :else 0))))

(def card->value-2 (zipmap [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A] (range 2 15)))

(defn compare-hands-2
  {:test (fn []
           (is (neg? (compare-hands "32T3K" "T55J5")))
           (is (pos? (compare-hands "KK677" "KTJJT"))))}
  [hand1 hand2]
  (let [score1 (get-score-2 hand1)
        score2 (get-score-2 hand2)]
    (if (not= score1 score2)
      (- score1 score2)
      (loop [[h1 & t1] hand1
             [h2 & t2] hand2]
        (let [v1 (card->value-2 h1)
              v2 (card->value-2 h2)]
          (if (= v1 v2)
            (recur t1 t2)
            (- v1 v2)))))))

(defn total-winnings-2
  {:test (fn [] (is= (total-winnings-2 test-input) 5905))}
  [input]
  (->> input
       (map (fn [r] (split r #" ")))
       (map (fn [[hand bid-as-sting]] {:hand hand :bid (read-string bid-as-sting)}))
       (sort-by :hand compare-hands-2)
       (map-indexed (fn [i {bid :bid}] (* (inc i) bid)))
       (reduce +)))

(deftest puzzle-b
  (is= (time (total-winnings-2 input))
       ; "Elapsed time: 67.694654 msecs"
       253253225))














