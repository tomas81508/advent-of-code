(ns advent-of-code.dec-2023.day-06
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not is]]
            [clojure.string :refer [split split-lines]]
            [clojure.math]))

; Together with Daniel Gullberg

(def input (-> (slurp "src/advent_of_code/dec_2023/day_06_input.txt")
               (split-lines)))

; t = t_1 + t_2 ; t_1 being the time charging
; d = t_2 * v = t_1 * t_2 = t_1 * (t - t_1) = - t_1^2 + t * t_1 > d_R

; Check when - t_1^2 + t * t_1 = d_R + 1 or
; t_1^2 - t * t_1 + d_R + 1 = 0
; t_1 = t/2 +- sqrt(t^2/4 - d_R - 1)

(defn interval-half-length
  [t d]
  (clojure.math/sqrt (- (/ (* t t) 4) (inc d))))

(defn integer-points-in-interval
  {:test (fn []
           (is= (integer-points-in-interval 7 9)
                4))}
  [t d]
  (let [l (interval-half-length t d)]
    (inc (int (- (clojure.math/floor (+ (/ t 2) l))
                 (clojure.math/ceil (- (/ t 2) l)))))))

(deftest puzzle-a
  (is= (time (let [ts (->> (re-seq #"\d+" (first input))
                           (map read-string))
                   ds (->> (re-seq #"\d+" (second input))
                           (map read-string))]
               (->> (map vector ts ds)
                    (reduce (fn [a v]
                              (* a (apply integer-points-in-interval v)))
                            1))))
       ; "Elapsed time: 0.335875 msecs"
       1195150))

(deftest puzzle-b
  (is= (time (let [t (->> (re-seq #"\d+" (first input))
                           (apply str)
                           (read-string))
                   d (->> (re-seq #"\d+" (second input))
                           (apply str)
                           (read-string))]
               (integer-points-in-interval t d)))
       ; "Elapsed time: 0.129292 msecs"
       42550411))

