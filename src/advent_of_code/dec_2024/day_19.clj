(ns advent-of-code.dec-2024.day-19
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.string :as string])
  (:import (java.util.regex Pattern)))

(def test-patterns ["r" "wr" "b" "g" "bwu" "rb" "gb" "br"])

(def test-designs ["brwrr" "bggr" "gbbr" "rrbgbr" "ubwu" "bwurrg" "brgr" "bbrgwb"])

(def patterns (-> (slurp "src/advent_of_code/dec_2024/day_19_input_1.txt")
                  (string/split #", ")))

(def designs (-> (slurp "src/advent_of_code/dec_2024/day_19_input_2.txt")
                 (string/split-lines)))


(defn create-material-pattern
  [patterns]
  (Pattern/compile (str "^(?:" (->> patterns (clojure.string/join "|")) ")+$")))

(def test-material-pattern (create-material-pattern test-patterns))
(def material-pattern (create-material-pattern patterns))

(defn possible-design?
  {:test (fn []
           (is (possible-design? test-material-pattern "brwrr")))}
  [material-pattern design]
  (re-find material-pattern design))

(defn count-possible-designs
  {:test (fn []
           (is= (count-possible-designs test-material-pattern test-designs)
                6))}
  [material-pattern designs]
  (->> designs
       (filter (fn [d] (possible-design? material-pattern d)))
       (count)))

(comment
  (time (count-possible-designs material-pattern designs))
  )

; part two

(declare number-of-different-ways-memoized)

(defn number-of-different-ways
  {:test (fn []
           (is= (number-of-different-ways test-patterns "brwrr") 2)
           (is= (number-of-different-ways test-patterns "rrbgbr") 6))}
  [patterns design]
  (if (= design "")
    1
    (let [result-patterns (->> patterns
                               (filter (fn [p] (string/starts-with? design p))))]
      (->> result-patterns
           (map (fn [p] (number-of-different-ways-memoized patterns (subs design (count p)))))
           (reduce +)))))

(def number-of-different-ways-memoized (memoize number-of-different-ways))

(comment
  (time (->> designs
             (map (fn [d] (number-of-different-ways-memoized patterns d)))
             (reduce +)))
  )

