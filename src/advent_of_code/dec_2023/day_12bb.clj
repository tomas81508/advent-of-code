(ns advent-of-code.dec-2023.day-12bb
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is is= is-not]]
            [clojure.string :refer [split split-lines]]
            [clojure.math]))


(def input (->> (slurp "src/advent_of_code/dec_2023/day_12_input.txt")
                (split-lines)))

(defn parse-info
  {:test (fn []
           (is= (parse-info "???.### 1,1,3")
                ["???.###????.###????.###????.###????.###" (list 1 1 3 1 1 3 1 1 3 1 1 3 1 1 3)]))}
  [s]
  (let [[springs ns] (split s #" ")]
    [(clojure.string/join "?" (repeat 5 springs))
     (->> (split ns #",")
          (map read-string)
          (apply list)
          (repeat 5)
          (flatten))]))

(defn string-contains? [s c] (some (fn [x] (= x c)) s))

(def maybe-operational #{\. \?})

(def maybe-damaged #{\? \#})

(declare count-configurations-memoized)

(defn count-configurations
  {:test (fn []
           (is= (apply count-configurations (parse-info "???.### 1,1,3"))
                1)
           (is= (apply count-configurations (parse-info ".??..??...?##. 1,1,3"))
                16384)
           )}
  [cfg nums]
  (cond (= cfg "")
        (if (empty? nums) 1 0)

        (empty? nums)
        (if (string-contains? cfg \#) 0 1)

        :else
        (let [term-1 (when (contains? maybe-operational (first cfg))
                       (count-configurations-memoized (subs cfg 1) nums))
              term-2 (when (contains? maybe-damaged (first cfg))
                       (let [n (first nums)]
                         (when (and (<= n (count cfg))
                                    (not (string-contains? (subs cfg 0 n) \.))
                                    (or (= n (count cfg))
                                        (not= (nth cfg n) \#)))
                           (count-configurations-memoized (if (= n (count cfg)) "" (subs cfg (inc n)))
                                                          (drop 1 nums)))))]
          (apply + 0 (remove nil? [term-1 term-2])))))

(def count-configurations-memoized (memoize count-configurations))

(deftest puzzle-b
  (is= (time (->> input
                  (map parse-info)
                  (map (fn [info] (apply count-configurations-memoized info)))
                  (reduce +)))
       ; "Elapsed time: 1290.278669 msecs"
       850504257483930))











