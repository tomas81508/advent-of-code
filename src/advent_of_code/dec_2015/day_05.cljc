(ns advent-of-code.dec-2015.day-05
  (:require [ysera.test :refer [is is-not is= deftest]]
            [clojure.string :refer [split-lines]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2015/day_05_input.txt")
       (split-lines)))


(defn contains-at-least-three-vowels?
  {:test (fn []
           (is (contains-at-least-three-vowels? "aei"))
           (is (contains-at-least-three-vowels? "xazegov"))
           (is (contains-at-least-three-vowels? "aeiouaeiouaeiou"))
           (is-not (contains-at-least-three-vowels? "abcde")))}
  [s]
  (>= (as-> (frequencies s) $
            (select-keys $ [\a \e \i \o \u])
            (vals $)
            (apply + $))
      3))


(defn contains-at-least-one-letter-that-appears-twice-in-a-row?
  {:test (fn []
           (is (contains-at-least-one-letter-that-appears-twice-in-a-row? "xx"))
           (is (contains-at-least-one-letter-that-appears-twice-in-a-row? "abcdde"))
           (is (contains-at-least-one-letter-that-appears-twice-in-a-row? "aabbccdd"))
           (is-not (contains-at-least-one-letter-that-appears-twice-in-a-row? "abcde"))
           (is-not (contains-at-least-one-letter-that-appears-twice-in-a-row? "abcabcabc")))}
  [s]
  (let [[l1 l2] s]
    (cond (nil? l2)
          false

          (= l1 l2)
          true

          :else
          (recur (subs s 1)))))

(defn contains-strange-strings?
  {:test (fn []
           (is (contains-strange-strings? "abcd"))
           (is (contains-strange-strings? "aaxybb"))
           (is-not (contains-strange-strings? "acpxbdqy")))}
  [s]
  (some (fn [l] (clojure.string/includes? s l))
        ["ab" "cd" "pq" "xy"]))


(defn nice?
  [s]
  (and (contains-at-least-three-vowels? s)
       (contains-at-least-one-letter-that-appears-twice-in-a-row? s)
       (not (contains-strange-strings? s))))

(deftest puzzle-a
         (is= (->> (get-puzzle-input)
                   (filter nice?)
                   (count))
              238))


(defn contains-pair-twice?
  {:test (fn []
           (is (contains-pair-twice? "xyxy"))
           (is-not (contains-pair-twice? "aaa")))}
  ([s] (contains-pair-twice? s #{} nil))
  ([s pairs previous-pair]
   (let [[l1 l2] s]
     (cond (contains? pairs [l1 l2])
           true

           (nil? l2)
           false

           :else
           (recur (subs s 1)
                  (if previous-pair
                    (conj pairs previous-pair)
                    pairs)
                  [l1 l2])))))


(defn contains-repeated-letters?
  {:test (fn []
           (is (contains-repeated-letters? "xyx"))
           (is (contains-repeated-letters? "abcdefeghi"))
           (is-not (contains-repeated-letters? "abcdefghi")))}
  [s]
  (let [[l1 _ l2] s]
    (cond (= l1 l2)
          true

          (nil? l2)
          false

          :else
          (recur (subs s 1)))))


(defn nice-2?
  [s]
  (and (contains-pair-twice? s)
       (contains-repeated-letters? s)))

(deftest puzzle-b
         (is= (->> (get-puzzle-input)
                   (filter nice-2?)
                   (count))
              69))