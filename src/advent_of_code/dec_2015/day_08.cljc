(ns advent-of-code.dec-2015.day-08
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :refer [split-lines] :as string]))


(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2015/day_08_input.txt")
       (split-lines)))

(defn get-test-input []
  (->> (slurp "src/advent_of_code/dec_2015/day_08_test_input.txt")
       (split-lines)))


(defn get-characters-of-code
  {:test (fn []
           (is= (get-characters-of-code (nth (get-test-input) 0)) 2)
           (is= (get-characters-of-code (nth (get-test-input) 1)) 5)
           (is= (get-characters-of-code (nth (get-test-input) 2)) 10)
           (is= (get-characters-of-code (nth (get-test-input) 3)) 6)
           (is= (get-characters-of-code (nth (get-test-input) 4)) 16)
           (is= (get-characters-of-code (nth (get-test-input) 5)) 42)
           (is= (get-characters-of-code (nth (get-test-input) 6)) 25)
           (is= (get-characters-of-code (nth (get-test-input) 7)) 8))}
  [s]
  (-> s
      (string/replace #"\\x[0-9a-f]{2}" "....")
      (string/replace #"\\\\" "..")
      (string/replace #"\\\"" "..")
      (string/replace #"\"" "..")
      (count)
      (- 2)))


(defn get-characters
  {:test (fn []
           (is= (get-characters (nth (get-test-input) 0)) 0)
           (is= (get-characters (nth (get-test-input) 1)) 3)
           (is= (get-characters (nth (get-test-input) 2)) 7)
           (is= (get-characters (nth (get-test-input) 3)) 1)
           (is= (get-characters (nth (get-test-input) 4)) 13)
           (is= (get-characters (nth (get-test-input) 5)) 29)
           (is= (get-characters (nth (get-test-input) 6)) 20))}
  [s]
  (as-> s $
        (subs $ 1 (dec (count $)))
        (string/replace $ #"\\x[0-9a-f]{2}" ".")
        (string/replace $ #"\\\\" ".")
        (string/replace $ #"\\\"" ".")
        (count $)))

(nth (get-puzzle-input) 292)

(deftest test-input
  (is= (->> (get-test-input)
            (take 4)
            (map (fn [s] (- (get-characters-of-code s)
                            (get-characters s))))
            (reduce +))
       12))

(deftest puzzle-a
  (is= (->> (get-puzzle-input)
            (map (fn [s] (- (get-characters-of-code s)
                            (get-characters s))))
            (reduce +))
       1333))

(defn get-encoded-strings
  {:test (fn []
           (is= (get-encoded-strings (nth (get-test-input) 0)) 6)
           (is= (get-encoded-strings (nth (get-test-input) 1)) 9)
           (is= (get-encoded-strings (nth (get-test-input) 2)) 16)
           (is= (get-encoded-strings (nth (get-test-input) 3)) 11))}
  [s]
  (->> s
       (map (fn [c] (condp = c
                      \" 2
                      \\ 2
                      1)))
       (reduce + 2)))

(deftest puzzle-b
  (is= (->> (get-puzzle-input)
            (map (fn [i] (- (get-encoded-strings i)
                            (get-characters-of-code i))))
            (reduce +))
       2046))
