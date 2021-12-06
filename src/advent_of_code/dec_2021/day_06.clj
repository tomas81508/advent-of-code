(ns advent-of-code.dec-2021.day-06
  (:require [ysera.test :refer [is is-not is= deftest]]))

; Initial state: 3,4,3,1,2
; After  1 day:  2,3,2,0,1
; After  2 days: 1,2,1,6,0,8
; After  3 days: 0,1,0,5,6,7,8
; After  4 days: 6,0,6,4,5,6,7,8,8
; After  5 days: 5,6,5,3,4,5,6,7,7,8
; After  6 days: 4,5,4,2,3,4,5,6,6,7
; After  7 days: 3,4,3,1,2,3,4,5,5,6
; After  8 days: 2,3,2,0,1,2,3,4,4,5
; After  9 days: 1,2,1,6,0,1,2,3,3,4,8
; After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
; After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
; After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
; After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
; After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
; After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
; After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
; After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
; After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8

(def test-input "3,4,3,1,2")

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_06_input.txt"))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {3 2 4 1 1 1 2 1}))}
  [input]
  (->> (clojure.string/split input #",")
       (map read-string)
       (frequencies)))

(defn next-day
  {:test (fn []
           (is= (->> (create-state test-input)
                     (next-day))
                {2 2 3 1 0 1 1 1})
           (is= (->> (create-state test-input)
                     (next-day)
                     (next-day))
                {1 2 2 1 6 1 0 1 8 1})
           (is= (->> (create-state test-input)
                     ((apply comp (repeat 3 next-day))))
                (create-state "0,1,0,5,6,7,8"))
           (is= (->> (create-state test-input)
                     ((apply comp (repeat 4 next-day))))
                (create-state "6,0,6,4,5,6,7,8,8"))
           (is= (->> (create-state test-input)
                     ((apply comp (repeat 18 next-day))))
                (create-state "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8")))}
  [state]
  (reduce-kv (fn [state k v]
               (cond (zero? k)
                     (-> state
                         (update 6 (fn [x] (+ (or x 0) v)))
                         (assoc 8 v))

                     (= k 7)
                     (update state 6 (fn [x] (+ (or x 0) v)))

                     :else
                     (assoc state (dec k) v)
                 ))
             {}
             state))

(deftest puzzle-a
         (is= (time (->> (create-state puzzle-input)
                         ((apply comp (repeat 80 next-day)))
                         (vals)
                         (reduce +)))
              ; "Elapsed time: 0.550389 msecs"
              380612))

(deftest puzzle-b
         (is= (time (->> (create-state puzzle-input)
                         ((apply comp (repeat 256 next-day)))
                         (vals)
                         (reduce +)))
              ; "Elapsed time: 0.998816 msecs"
              1710166656900))
