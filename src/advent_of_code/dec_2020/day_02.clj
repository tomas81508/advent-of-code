(ns advent-of-code.dec-2020.day-02
  (:require [ysera.test :refer [is is-not is= deftest]]))


(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_02.txt")
       (clojure.string/split-lines)))

(defn valid-password?
  {:test (fn []
           (is (valid-password? "1-3 a" "abcde"))
           (is-not (valid-password? "1-3 b" "cdefg"))
           (is (valid-password? "2-9 c" "ccccccccc")))}
  [policy password]
  (let [[_ min-as-str max-as-str letter] (re-find (re-pattern "([\\d]+)-([\\d]+) (\\w)") policy)
        min-occur (read-string min-as-str)
        max-occur (read-string max-as-str)]
    (as-> (frequencies password) $
          (or (get $ (.charAt letter 0)) 0)
          (<= min-occur $ max-occur))))

(deftest puzzle-2a
         (is= (->> (get-puzzle-input)
                   (map (fn [text] (clojure.string/split text (re-pattern ": "))))
                   (filter (fn [[policy password]] (valid-password? policy password)))
                   (count))
              660))

(defn valid-password-2?
  {:test (fn []
           (is (valid-password-2? "1-3 a" "abcde"))
           (is-not (valid-password-2? "1-3 b" "cdefg"))
           (is-not (valid-password-2? "2-9 c" "ccccccccc")))}
  [policy password]
  (let [[_ min-as-str max-as-str letter] (re-find (re-pattern "([\\d]+)-([\\d]+) (\\w)") policy)
        min-index (read-string min-as-str)
        max-index (read-string max-as-str)
        at-position-min (= (str (nth password (dec min-index))) letter)
        at-position-max (= (str (nth password (dec max-index))) letter)]
    (or (and at-position-max (not at-position-min))
        (and (not at-position-max) at-position-min))))

(deftest puzzle-2b
         (is= (->> (get-puzzle-input)
                   (map (fn [text] (clojure.string/split text (re-pattern ": "))))
                   (filter (fn [[policy password]] (valid-password-2? policy password)))
                   (count))
              530))
