(ns advent-of-code.dec-2016.day-13
  (:require [ysera.test :refer [deftest is= is is-not]]))


(def puzzle-input 1350)

(defn f
  [x y]
  (+ (* x x) (* 3 x) (* 2 x y) y (* y y)))

(defn binary-representation
  [n]
  (Integer/toBinaryString n))

(defn open-space?
  {:test (fn []
           (is (open-space? [0 0] 10))
           (is (open-space? [4 4] 10))
           (is (open-space? [1 6] 10))
           (is (open-space? [9 5] 10))
           (is-not (open-space? [1 0] 10))
           (is-not (open-space? [8 3] 10))
           (is-not (open-space? [5 4] 10))
           (is-not (open-space? [0 6] 10)))}
  [[x y] favorite-number]
  (let [n (+ (f x y) favorite-number)]
    (->> (binary-representation n)
         (filter (fn [x] (= x \1)))
         (count)
         (even?))))

(def open-space? (memoize open-space?))

(defn walk
  {:test (fn []
           (is= (walk [1 1] {[1 1] 0} 10)
                {[0 1] 1
                 [1 1] 0
                 [1 2] 1})
           (is= (walk [1 2] {[0 1] 1 [1 2] 1 [1 1] 0} 10)
                {[0 1] 1
                 [1 2] 1
                 [1 1] 0
                 [2 2] 2}))}
  [position distances favorite-number]
  (let [distance (get distances position)
        neighbors (->> [[-1 0] [0 1] [1 0] [0 -1]]
                       (map (fn [d] (mapv + position d))))]
    (->> neighbors
         (remove (fn [n] (or (neg? (first n))
                             (neg? (second n))
                             (contains? distances n)
                             (not (open-space? n favorite-number)))))
         (reduce (fn [a v]
                   (assoc a v (inc distance)))
                 distances))))

(defn extend-walk
  {:test (fn []
           (is= (extend-walk {[1 1] 0} 0 10)
                {[0 1] 1 [1 1] 0 [1 2] 1})
           (is= (extend-walk {[0 1] 1 [1 1] 0 [1 2] 1} 1 10)
                {[0 1] 1 [1 2] 1 [1 1] 0 [2 2] 2 [0 0] 2}))}
  [distances distance favorite-number]
  (->> (seq distances)
       (filter (fn [[_ d]] (= d distance)))
       (reduce (fn [a [position _]]
                 (walk position a favorite-number))
               distances)))

(defn walk-to
  {:test (fn []
           (is= (walk-to {[1 1] 0} [7 4] 10)
                11))}
  [distances goal favorite-number]
  (loop [distance 0
         distances distances]
    (if-let [result (get distances goal)]
      result
      (recur (inc distance)
             (extend-walk distances distance favorite-number)))))

(deftest puzzle-a
         (is= (time (walk-to {[1 1] 0} [31 39] 1350))
              ; "Elapsed time: 11.93794 msecs"
              92))

(deftest puzzle-b
         (is= (time (->> (range 0 50)
                         (reduce (fn [distances distance]
                                   (extend-walk distances distance 1350))
                                 {[1 1] 0})
                         (keys)
                         (count)))
              ; "Elapsed time: 2.506494 msecs"
              124))
