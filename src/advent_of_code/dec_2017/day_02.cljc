(ns advent-of-code.dec-2017.day-02
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]))

(defn max-difference
  {:test (fn []
           (is= (max-difference "5  1  9  5") 8)
           (is= (max-difference "7 5 3") 4)
           (is= (max-difference "2\t4\t6\t8") 6)
           (is= (max-difference "12 4 6 28") 24))}
  [input]
  (let [number-seq (->> (string/split input #"[ ]+|\t")
                        (map (fn [l] (read-string (str l)))))]
    (- (apply max number-seq) (apply min number-seq))))

(defn max-div
  {:test (fn []
           (is= (max-div "5\t9\t2\t8") 4)
           (is= (max-div "9 4 7 3") 3)
           (is= (max-div "3 8 6 5") 2))}
  [input]
  (let [number-seq (->> (string/split input #"[ ]+|\t")
                        (map (fn [l] (read-string (str l)))))]
    (reduce (fn [a t]
              (let [value (reduce (fn [a n]
                                    (let [value (/ t n)]
                                      (if (and (integer? value)
                                               (> value a))
                                        value
                                        a)))
                                  1
                                  number-seq)]
                (if (> value a) value a)))
            1
            number-seq)))

(defn checksum
  {:test (fn []
           (is= (checksum "5 1 9 5\n7 5 3\n2 4 6 8"
                          max-difference)
                18))}
  [input strategy-fn]
  (reduce (fn [a v]
            (+ a (strategy-fn v)))
          0
          (string/split-lines input)))

(deftest puzzle-a
  (is= (checksum (slurp "src/advent_of_code/dec_2017/day_02_input.txt") max-difference)
       42378))

(deftest puzzle-b
  (is= (checksum (slurp "src/advent_of_code/dec_2017/day_02_input.txt") max-div)
       246))





