(ns advent-of-code.dec-2018.day-02
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.collections :refer [seq-contains?]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2018/day_02.txt")
       (clojure.string/split-lines)))

(defn get-frequencies
  {:test (fn []
           (is= (get-frequencies "abcdef") [0 0])
           (is= (get-frequencies "bababc") [1 1])
           (is= (get-frequencies "abbcde") [1 0])
           (is= (get-frequencies "abcccd") [0 1])
           (is= (get-frequencies "aabcdd") [1 0])
           (is= (get-frequencies "abcdee") [1 0])
           (is= (get-frequencies "ababab") [0 1]))}
  [input]
  (let [counts (-> input
                   (frequencies)
                   (vals))
        contains-fn (fn [coll n]
                      (if (seq-contains? coll n) 1 0))]
    [(contains-fn counts 2) (contains-fn counts 3)]))


(defn checksum
  {:test (fn []
           (is= (checksum ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]) 12))}
  [input]
  (->> input
       (map get-frequencies)
       (apply map +)
       (apply *)))

(deftest puzzle-part-1
  (is= (checksum (get-puzzle-input)) 8610))

(defn get-matching-chars
  {:test (fn []
           (is= (get-matching-chars "abc" "abd") "ab")
           (is= (get-matching-chars "abc" "adb") "a")
           (is= (get-matching-chars "abb" "bad") ""))}
  [id1 id2]
  (reduce (fn [a i]
            (let [l1 (nth id1 i)
                  l2 (nth id2 i)]
              (if (= l1 l2) (str a l1) a)))
          ""
          (range (count id1))))

(defn find-match
  {:test (fn []
           (is= (find-match "abc" ["abd" "aaa"]) "ab")
           (is= (find-match "abc" ["bbd" "aaa"]) nil)
           )}
  [candidate ids]
  (->> ids
       (map (fn [id] (get-matching-chars candidate id)))
       (filter (fn [id] (= (dec (count candidate)) (count id))))
       (first)))


(defn common-letters
  [[h & t]]
  (if-let [match (find-match h t)]
    match
    (recur t)))


(deftest puzzle-part-2
  (is= (common-letters (get-puzzle-input))
       "iosnxmfkpabcjpdywvrtahluy"))