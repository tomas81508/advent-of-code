(ns advent-of-code.dec-2015.day-20
  (:require [advent-of-code.test :refer [is=]]))

(def input 33100000)

(defn find-factors
  {:test (fn []
           (is= (find-factors 8) #{1 2 4 8})
           (is= (find-factors 11) #{1 11}))}
  [n]
  (->> (range 1 (inc (Math/sqrt n)))
       (filter (fn [x] (zero? (rem n x))))
       (reduce (fn [a v] (conj a v (/ n v))) #{})))

(defn count-presents
  {:test (fn []
           (is= (count-presents 6) 120)
           (is= (count-presents 9) 130))}
  [house]
  (* 10 (reduce + (find-factors house))))

(defn part-1
  []
  (->> (range)
       (map (fn [house] [house (count-presents house)]))
       (some (fn [[house presents]] (when (>= presents input) house)))))

(comment
  (time (part-1))
  ; "Elapsed time: 5846.002208 msecs"
  ; => 776160
  )

(defn count-presents-2
  [house]
  (let [factors (->> (find-factors house)
                     (remove (fn [factor] (< (* 50 factor) house))))]
    (* 11 (reduce + factors))))

(defn part-2
  []
  (->> (range)
       (map (fn [house] [house (count-presents-2 house)]))
       (some (fn [[house presents]] (when (>= presents input) house)))))


(comment
  (time (part-2))
  ; "Elapsed time: 6740.973625 msecs"
  ; => 786240
  )
