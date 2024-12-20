(ns advent-of-code.dec-2019.day-22
  (:require [advent-of-code.test :refer [is is= is-not]]
            [clojure.test :refer [deftest]]))


(defn new-stack
  {:test (fn []
           (is= (new-stack [0 1 2 3 4 5 6 7 8 9])
                [9 8 7 6 5 4 3 2 1 0]))}
  [deck]
  (vec (reverse deck)))

(defn cut-N
  {:test (fn []
           (is= (cut-N [0 1 2 3 4 5 6 7 8 9] 3)
                [3 4 5 6 7 8 9 0 1 2])
           (is= (cut-N [0 1 2 3 4 5 6 7 8 9] -4)
                [6 7 8 9 0 1 2 3 4 5]))}
  [deck n]
  (cond (= n 0) deck

        (pos? n)
        (let [[fs rs] (split-at n deck)]
          (vec (concat rs fs)))

        :else
        (-> deck
            (reverse)
            (cut-N (- n))
            (reverse)
            (vec))))

(defn increment-N
  {:test (fn []
           (is= (increment-N [0 1 2 3 4 5 6 7 8 9] 3)
                [0 7 4 1 8 5 2 9 6 3]))}
  [deck n]
  (let [size (count deck)]
    (->> deck
         ;(map-indexed (fn [index value] [index value]))
         (reduce (fn [a value]
                   (-> a
                       (assoc-in [:deck (:current-index a)] value)
                       (assoc :current-index (mod (+ (:current-index a) n) size))))
                 {:current-index 0
                  :deck          deck})
         (:deck))))

(deftest test-1
  (is= (-> [0 1 2 3 4 5 6 7 8 9]
           (increment-N 7)
           (new-stack)
           (new-stack))
       [0 3 6 9 2 5 8 1 4 7]))

(deftest test-2
  (is= (-> [0 1 2 3 4 5 6 7 8 9]
           (cut-N 6)
           (increment-N 7)
           (new-stack))
       [3 0 7 4 1 8 5 2 9 6]))

(deftest test-3
  (is= (-> [0 1 2 3 4 5 6 7 8 9]
           (increment-N 7)
           (increment-N 9)
           (cut-N -2))
       [6 3 0 7 4 1 8 5 2 9]))

(deftest test-4
  (is= (-> [0 1 2 3 4 5 6 7 8 9]
           (new-stack)
           (cut-N -2)
           (increment-N 7)
           (cut-N 8)
           (cut-N -4)
           (increment-N 7)
           (cut-N 3)
           (increment-N 9)
           (increment-N 3)
           (cut-N -1))
       [9 2 5 8 1 4 7 0 3 6]))

(def puzzle-deck (vec (range 10007)))

(def puzzle-input (->> (slurp "src/advent_of_code/dec_2019/day_22.txt")
                       (clojure.string/split-lines)))

(defn solver-a
  {:test (fn []
           (is= (solver-a [0 1 2 3 4 5 6 7 8 9]
                          ["cut 6"
                           "deal with increment 7"
                           "deal into new stack"])
                [3 0 7 4 1 8 5 2 9 6]))}
  [deck instructions]
  (let [cut-N-pattern (re-pattern "cut ([-]*\\d+)")
        increment-N-pattern (re-pattern "deal with increment (\\d+)")]
    (reduce (fn [deck instruction]
              (cond (clojure.string/starts-with? instruction "cut")
                    (let [[_ n] (re-matches cut-N-pattern instruction)]
                      (cut-N deck (read-string n)))

                    (clojure.string/starts-with? instruction "deal with increment")
                    (let [[_ n] (re-matches increment-N-pattern instruction)]
                      (increment-N deck (read-string n)))

                    (= instruction "deal into new stack")
                    (new-stack deck)

                    ))
            deck
            instructions)))

(deftest puzzle-a
  (is= (time (->> (solver-a puzzle-deck puzzle-input)
                  (reduce (fn [a v]
                            (if (= v 2019)
                              (reduced a)
                              (inc a)))
                          0)))
       ; "Elapsed time: 187.189467 msecs"
       6417))



