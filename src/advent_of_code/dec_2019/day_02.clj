(ns advent-of-code.dec-2019.day-02
  (:require [ysera.test :refer [is= deftest]]))


(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_02.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (partition 4 $)
        (map vec $)
        (vec $)))

(defn get-value-at-position
  {:test (fn []
           (is= (get-value-at-position [[1 9 10 3]
                                        [2 3 11 0]
                                        [99 30 40 50]]
                                       9)
                30)
           (is= (get-value-at-position [[1 9 10 3]
                                        [2 3 11 0]
                                        [99 30 40 50]]
                                       0)
                1))}
  [program position]
  (let [line-index (quot position 4)
        step-index (mod position 4)]
    (get-in program [line-index step-index])))

(defn run
  {:test (fn []
           (is= (run [[1 9 10 3] [2 3 11 0] [99 30 40 50]])
                [[3500 9 10 70] [2 3 11 0] [99 30 40 50]])
           (is= (run [[1 0 0 0] [99]])
                [[2 0 0 0] [99]])
           (is= (run [[2 3 0 3] [99]])
                [[2 3 0 6] [99]])
           (is= (run [[2 4 4 5] [99 0]])
                [[2 4 4 5] [99 9801]])
           (is= (run [[1 1 1 4] [99 5 6 0] [99]])
                [[30 1 1 4] [2 5 6 0] [99]]))}
  ([program noun verb]
   (loop [program (-> program
                      (update-in [0 1] (fn [v] (or noun v)))
                      (update-in [0 2] (fn [v] (or verb v))))
          line-index 0]
     (let [[Opcode i1 i2 i3] (nth program line-index)]
       (if (= Opcode 99)
         program
         (let [v1 (get-value-at-position program i1)
               v2 (get-value-at-position program i2)
               operation (condp = Opcode 1 + 2 *)
               insert-line-index (quot i3 4)
               insert-step-index (mod i3 4)]
           (recur (update program insert-line-index
                          (fn [line]
                            (assoc line insert-step-index (operation v1 v2))))
                  (inc line-index)))))))
  ([program]
   (run program nil nil)))

(deftest puzzle-2a
         (is= (-> (get-puzzle-input)
                  (assoc-in [0 1] 12)
                  (assoc-in [0 2] 2)
                  (run)
                  (ffirst))
              3058646))

(defn solve
  {:test (fn []
           (is= (solve (get-puzzle-input) 3058646)
                {:noun 12 :verb 2}))}
  [program output]
  (let [parameters (for [noun (range 100) verb (range 100)]
                     [noun verb])
        index (->> parameters
                   (map-indexed (fn [index [noun verb]] [index (ffirst (run program noun verb))]))
                   (filter (fn [[_ result]] (= result output)))
                   (first)
                   (first))
        [noun verb] (nth parameters index)]
    {:noun noun :verb verb}))

(deftest puzzle-2b
         (is= (let [{noun :noun
                     verb :verb} (solve (get-puzzle-input) 19690720)]
                (+ (* 100 noun) verb))
              8976))
