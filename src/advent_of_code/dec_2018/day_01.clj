(ns advent-of-code.dec-2018.day-01
  (:require [ysera.test :refer [is= deftest]]))

(defn solve-part-1
  {:test (fn []
           (is= (solve-part-1 [3 3 4 -2 -4]) 4)
           (is= (solve-part-1 [1 1 1]) 3))}
  [input]
  (apply + input))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2018/day_01.txt")
       (clojure.string/split-lines)
       (map read-string)))

(deftest puzzle-part-1
         (is= (-> (get-puzzle-input)
                  (solve-part-1))
              529))

(defn solve-part-2a
  {:test (fn []
           (is= (solve-part-2a [1 -1]) 0)
           (is= (solve-part-2a [3 3 4 -2 -4]) 10))}
  [input]
  (reduce (fn [a v]
            (let [next-sum (+ v (:current-sum a))
                  all-sums (:all-sums a)]
              (if (contains? all-sums next-sum)
                (reduced next-sum)
                {:current-sum next-sum
                 :all-sums    (conj all-sums next-sum)})))
          {:current-sum 0
           :all-sums    #{0}}
          (flatten (repeat input))))

(defn solve-part-2b
  {:test (fn []
           (is= (solve-part-2b [1 -1]) 0)
           (is= (solve-part-2b [3 3 4 -2 -4]) 10))}
  [input]
  (loop [current-sum 0
         all-sums    #{0}
         input (flatten (repeat input))]
    (let [next-sum (+ current-sum (first input))]
      (if (contains? all-sums next-sum)
        next-sum
        (recur next-sum
               (conj all-sums next-sum)
               (drop 1 input))))))

(deftest puzzle-part-2
         (is= (-> (get-puzzle-input)
                  (solve-part-2a))
              464)
         (is= (-> (get-puzzle-input)
                  (solve-part-2b))
              464))
