(ns advent-of-code.dec-2017.day-04
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]))

(defn valid-passphrase
  {:test (fn []
           (is (valid-passphrase "aa bb cc"))
           (is (valid-passphrase "aa bb cc aaa"))
           (is-not (valid-passphrase "aa bb cc aa")))}
  [phrase]
  (let [words (string/split phrase #" ")
        unique-words (into #{} words)]
    (= (count words) (count unique-words))))

(deftest puzzle-a
  (is= (->> (slurp "src/advent_of_code/dec_2017/day_04_input.txt")
            (string/split-lines)
            (filter valid-passphrase)
            (count))
       455))

(defn valid-improved-passphrase
  {:test (fn []
           (is (valid-improved-passphrase "abcde fghij"))
           (is-not (valid-improved-passphrase "abcde xyz ecdab"))
           (is (valid-improved-passphrase "a ab abc abd abf abj"))
           (is (valid-improved-passphrase "iiii oiii ooii oooi oooo"))
           (is-not (valid-improved-passphrase "oiii ioii iioi iiio")))}
  [phrase]
  (let [words (string/split phrase #" ")
        sorted-words (map (fn [word] (sort word)) words)
        unique-words (into #{} sorted-words)]
    (= (count sorted-words) (count unique-words))))

(deftest puzzle-b
  (is= (->> (slurp "src/advent_of_code/dec_2017/day_04_input.txt")
            (string/split-lines)
            (filter valid-improved-passphrase)
            (count))
       186))


















