(ns advent-of-code.dec-2024.day-05
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is is-not]]))

(def input-rules (->> (slurp "src/advent_of_code/dec_2024/day_05_input-rules.txt")
                      (clojure.string/split-lines)))

(def input-updates (->> (slurp "src/advent_of_code/dec_2024/day_05_input-updates.txt")
                        (clojure.string/split-lines)))

(def test-input-rules ["47|53" "97|13" "97|61" "97|47" "75|29" "61|13" "75|53" "29|13" "97|29" "53|29" "61|53" "97|53" "61|29" "47|13" "75|47" "97|75" "47|61" "75|61" "47|29" "75|13" "53|13"])

(def test-input-updates ["75,47,61,53,29"
                         "97,61,53,29,13"
                         "75,29,13"
                         "75,97,47,61,53"
                         "61,13,29"
                         "97,13,75,29,47"])

(defn create-rules [input]
  (->> input
       (map (fn [r] (map read-string (re-seq #"\d+" r))))
       (reduce (fn [a [k v]]
                 (update a k (fn [old-value] (if old-value (conj old-value v) #{v}))))
               {})))

(def test-rules (create-rules test-input-rules))
(def rules (create-rules input-rules))

(defn create-page-updates
  [input]
  (->> input
       (map (fn [r] (map read-string (re-seq #"\d+" r))))))

(def test-page-updates (create-page-updates test-input-updates))
(def page-updates (create-page-updates input-updates))

(defn valid-update?
  {:test (fn []
           (is (valid-update? test-rules [75 47 61 53 29]))
           (is (valid-update? test-rules [97, 61, 53, 29, 13]))
           (is (valid-update? test-rules [75, 29, 13]))
           (is-not (valid-update? test-rules [75, 97, 47, 61, 53]))
           (is-not (valid-update? test-rules [61, 13, 29]))
           (is-not (valid-update? test-rules [97, 13, 75, 29, 47])))}
  [rules page-update]
  (loop [[f & r] (reverse page-update)]
    (if-not f
      true
      (let [current-rule-set (get rules f)
            test-set (into #{} r)]
        (if (empty? (clojure.set/intersection current-rule-set test-set))
          (recur r)
          false)))))

(defn get-middle-thing
  {:test (fn []
           (is= (get-middle-thing [1 4 6 7 9]) 6))}
  [l]
  (let [length (count l)]
    (nth l (/ (dec length) 2))))


(defn calculate-part-1
  {:test (fn []
           (is= (calculate-part-1 test-rules test-page-updates)
                143))}
  [rules page-updates]
  (->> page-updates
       (filter (fn [page-update] (valid-update? rules page-update)))
       (map get-middle-thing)
       (reduce +)))

(comment
  (reduce + (range 10))
  (reduce + 0 (range 10))
  (reduce (fn [a v] (+ a v)) 0 (range 10))

  (time (calculate-part-1 rules page-updates))
  )

(defn sort-pages
  {:test (fn []
           (is= (sort-pages test-rules [75, 97, 47, 61, 53])
                [97, 75, 47, 61, 53])
           (is= (sort-pages test-rules [61, 13, 29])
                [61, 29, 13])
           (is= (sort-pages test-rules [97, 13, 75, 29, 47])
                [97, 75, 47, 29, 13]))}
  [rules page-update]
  (-> (loop [[f & r] (reverse page-update)
             result []]
        (if-not f
          result
          (let [current-rule-set (get rules f)
                test-set (into #{} r)]
            (if (empty? (clojure.set/intersection current-rule-set test-set))
              (recur r (conj result f))
              (recur (concat r [f]) result)))))
      (reverse)))

(defn calculate-part-2
  {:test (fn []
           (is= (calculate-part-2 test-rules test-page-updates)
                123))}
  [rules page-updates]
  (->> page-updates
       (remove (fn [page-update] (valid-update? rules page-update)))
       (map (fn [pu] (sort-pages rules pu)))
       (map get-middle-thing)
       (reduce +)))

(comment
  (time (calculate-part-2 rules page-updates))
  )




