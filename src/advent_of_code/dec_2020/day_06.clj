(ns advent-of-code.dec-2020.day-06
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input []
  (slurp "src/advent_of_code/dec_2020/day_06.txt"))

(def test-input "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

(defn transform-input
  {:test (fn []
           (is= (transform-input test-input)
                [["abc"]
                 ["a" "b" "c"]
                 ["ab" "ac"]
                 ["a" "a" "a" "a"]
                 ["b"]]))}
  [input]
  (as-> input $
        (clojure.string/split $ #"\n\n")
        (map (fn [group-as-string]
               (clojure.string/split group-as-string #"\n"))
             $)))

(defn answer-counts
  {:test (fn []
           (is= (answer-counts ["abc"]) 3)
           (is= (answer-counts ["a" "b" "c"]) 3)
           (is= (answer-counts ["ab" "ac"]) 3)
           (is= (answer-counts ["a" "a" "a" "a"]) 1)
           (is= (answer-counts ["b"]) 1))}
  [group-data]
  (->> group-data
       (map seq)
       (apply concat)
       (set)
       (count)))

(deftest puzzle-a
  (is= (->> (get-puzzle-input)
            (transform-input)
            (map answer-counts)
            (apply +))
       6534))

(defn all-answer-counts
  {:test (fn []
           (is= (all-answer-counts ["abc"]) 3)
           (is= (all-answer-counts ["a" "b" "c"]) 0)
           (is= (all-answer-counts ["ab" "ac"]) 1)
           (is= (all-answer-counts ["a" "a" "a" "a"]) 1)
           (is= (all-answer-counts ["b"]) 1))}
  [group-data]
  (let [group-size (count group-data)]
    (->> group-data
         (map seq)
         (apply concat)
         (frequencies)
         (reduce-kv (fn [a _ v]
                      (if (= v group-size)
                        (inc a)
                        a))
                    0))))

(deftest puzzle-b
  (is= (->> (get-puzzle-input)
            (transform-input)
            (map all-answer-counts)
            (apply +))
       3402))










