(ns advent-of-code.dec-2015.day-12
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.data.json :refer [read-str]]))


(deftest puzzle-a
  (is= (time (let [pattern #"([-|\d]+)(.*)"]
               (loop [s (slurp "src/advent_of_code/dec_2015/day_12_input.txt")
                      sum 0]
                 (let [[_ n r] (re-find pattern s)]
                   (if (nil? n)
                     sum
                     (recur r (+ sum (read-string n))))))))
       ; "Elapsed time: 308.499628 msecs"
       156366))


(defn find-sum
  {:test (fn []
           (is= (find-sum -44) -44)
           (is= (find-sum {"e" -44, "a" 33, "d" "violet", "c" 188, "h" "red", "b" -36, "g" "blue", "f" "yellow"})
                141)
           )}
  [entity]
  (cond (number? entity)
        entity

        (string? entity)
        0

        (vector? entity)
        (reduce + (map find-sum entity))

        (map? entity)
        (reduce + (map find-sum (vals entity)))))

(deftest puzzle-a-alt-2
  (is= (time (find-sum (read-str (slurp "src/advent_of_code/dec_2015/day_12_input.txt"))))
       ; "Elapsed time: 5.216753 msecs"
       156366))


(defn find-sum-without-red
  {:test (fn []
           (is= (find-sum-without-red -44) -44)
           (is= (find-sum-without-red {"e" -44, "a" 33, "d" "red", "c" 188, "h" "red", "b" -36, "g" "blue", "f" "yellow"})
                0)
           )}
  [entity]
  (cond (number? entity)
        entity

        (string? entity)
        0

        (vector? entity)
        (reduce + (map find-sum-without-red entity))

        (map? entity)
        (if (some (fn [v] (= v "red")) (vals entity))
          0
          (reduce + (map find-sum-without-red (vals entity))))))

(deftest puzzle-b
  (is= (time (find-sum-without-red (read-str (slurp "src/advent_of_code/dec_2015/day_12_input.txt"))))
       ; "Elapsed time: 4.860071 msecs"
       96852))