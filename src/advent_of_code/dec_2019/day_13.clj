(ns advent-of-code.dec-2019.day-13
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.dec-2019.day-09 :refer [run
                                                    run-instruction
                                                    create-program]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_13.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)))

(deftest puzzle-a
  (is= (->> (get-puzzle-input)
            (run)
            (:outputs)
            (partition 3)
            (reduce (fn [a [x y id]]
                      (assoc a [x y] id))
                    {})
            (vals)
            (filter (fn [v] (= v 2)))
            (count))
       228))

(deftest puzzle-b
  (is= (time (let [program (as-> (get-puzzle-input) $
                                 (assoc $ 0 2)
                                 (create-program $ []))]
               (loop [program program
                      paddle nil
                      ball nil
                      score 0]
                 (let [inputs (if (and paddle ball)
                                [(cond (< (first paddle) (first ball)) 1
                                       (> (first paddle) (first ball)) -1
                                       :else 0)]
                                [0])
                       program (-> (assoc program :inputs inputs)
                                   (run-instruction))]
                   (cond (:halted program)
                         score

                         (= (count (:outputs program)) 3)
                         (let [[x y z] (:outputs program)]
                           (recur (assoc program :outputs [])
                                  (if (= z 3) [x y] paddle)
                                  (if (= z 4) [x y] ball)
                                  (if (and (= x -1) (= y 0)) z score)))

                         :else
                         (recur program
                                paddle
                                ball
                                score))))))
       ; "Elapsed time: 1863.115829 msecs"
       10776))