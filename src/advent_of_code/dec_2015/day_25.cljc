(ns advent-of-code.dec-2015.day-25
  (:require [advent-of-code.test :refer [is=]]))

(def input {:row    2981
            :column 3075})

(def s 20151125)
(def m 252533)
(def d 33554393)
(def r 31916031)

(defn next-number
  {:test (fn []
           (is= (next-number 20151125) 31916031)
           (is= (next-number 31916031) 18749137))}
  [n]
  (rem (* n m) d))

(defn find-index
  {:test (fn []
           (is= (find-index 1 1) 1)
           (is= (find-index 4 2) 12)
           (is= (find-index 2 4) 14)
           (is= (find-index 4 3) 18)
           (is= (find-index 3 4) 19)
           (is= (find-index 1 6) 21))}
  [row column]
  (+ (reduce (fn [a c] (+ a row (- column c)))
             0
             (range 1 (inc column)))
     (reduce (fn [a c] (+ a c 1))
             0
             (range (- row 2)))))

(defn part-1
  [input]
  (let [index (find-index (:row input) (:column input))]
    (loop [i 1
           n s]
      (if (= i index)
        n
        (recur (inc i) (next-number n))))))

(comment
  (time (part-1 input))
  ; "Elapsed time: 196.501542 msecs"
  ; => 9132360
  )
