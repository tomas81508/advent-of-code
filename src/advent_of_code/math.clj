(ns advent-of-code.math
  (:require [advent-of-code.test :refer [is=]]))

(defn round
  {:test (fn []
           (is= (round 2.4) 2)
           (is= (round 2.5) 3))}
  [n]
  (Math/round (double n)))

(defn floor
  {:test (fn []
           (is= (floor 2.4) 2)
           (is= (floor 2.5) 2))}
  [x]
  (int (Math/floor (double x))))

(defn ceil
  {:test (fn []
           (is= (ceil 2.4) 3)
           (is= (ceil 2.5) 3))}
  [x]
  (int (Math/ceil (double x))))
