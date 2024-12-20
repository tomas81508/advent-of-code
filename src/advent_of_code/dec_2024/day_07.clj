(ns advent-of-code.dec-2024.day-07
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.math.combinatorics :refer [permuted-combinations]]
            [clojure.math]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_07_input.txt")
                (clojure.string/split-lines)))

(def test-input ["190: 10 19"
                 "3267: 81 40 27"
                 "83: 17 5"
                 "156: 15 6"
                 "7290: 6 8 6 15"
                 "161011: 16 10 13"
                 "192: 17 8 14"
                 "21037: 9 7 18 13"
                 "292: 11 6 16 20"])

(def test-equations (->> test-input
                         (map (fn [equation] (map read-string (re-seq #"\d+" equation))))))

(def equations (->> input
                    (map (fn [equation] (map read-string (re-seq #"\d+" equation))))))

(def operators-1 ['+ '*])

(defn make-combinations
  {:test (fn []
           (is= (make-combinations 1 operators-1)
                [['+] ['*]])
           (is= (make-combinations 2 operators-1)
                [['+ '+] ['+ '*] ['* '+] ['* '*]]))}
  [n operators]
  (-> (repeat n operators)
      (flatten)
      (permuted-combinations n)))

(defn equation-valid?
  {:test (fn []
           (is (equation-valid? 190 [19 10] ['*]))
           (is-not (equation-valid? 190 [19 10] ['+])))}
  [result numbers operators]
  (= result
     (->> (map vector operators (drop 1 numbers))
          (reduce (fn [a [op n]]
                    (if (> a result)
                      (reduced :no-solution)
                      ((eval op) a n)))
                  (first numbers)))))

(defn evaluate-equation
  {:test (fn []
           (is (evaluate-equation [190 10 19] operators-1))
           (is-not (evaluate-equation [7290 6 8 6 15] operators-1)))}
  [[result & numbers] operators]
  (let [operator-combinations (make-combinations (dec (count numbers)) operators)]
    (loop [[oc & ocs] operator-combinations]
      (if (nil? oc)
        nil
        (if (equation-valid? result numbers oc)
          true
          (recur ocs))))))

(defn total-calibration-result
  {:test (fn []
           (is= (total-calibration-result test-equations operators-1)
                3749))}
  [equations operators]
  (->> equations
       (keep (fn [e] (when (evaluate-equation e operators) (first e))))
       (reduce +)))

(defn concatenation
  {:test (fn []
           (is= (concatenation 123 456) 123456))}
  [x y]
  (read-string (str x y)))

(def operators-2 (conj operators-1 'concatenation))

(comment
  (time (total-calibration-result equations operators-1))
  ; "Elapsed time: 5578.216645 msecs"
  ;   => 8401132154762
  (time (total-calibration-result equations operators-2))
  ; "Elapsed time: 288355.003395 msecs"
  ; => 95297119227552
  )

