(ns advent-of-code.dec-2024.day-01
  (:require [advent-of-code.test :refer [is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_01_input.txt")
                (clojure.string/split-lines)))

(def test-input ["3   4"
                 "4   3"
                 "2   5"
                 "1   3"
                 "3   9"
                 "3   3"])

(defn total-distance
  {:test (fn []
           (is= (total-distance test-input) 11))}
  [input]
  (->> input
       (map (fn [l] (re-seq #"\d+" l)))
       (reduce (fn [a [le re]]
                 (-> a
                     (update :left conj (read-string le))
                     (update :right conj (read-string re))))
               {:left  []
                :right []})
       (vals)
       (map sort)
       (apply map -)
       (map abs)
       (reduce +)))

(comment
  (time (total-distance input))
  )

; Part Two

(defn similarity-score
  {:test (fn []
           (is= (similarity-score test-input) 31))}
  [input]
  (let [[first-frequencies second-frequencies] (->> input
                                                    (map (fn [l] (re-seq #"\d+" l)))
                                                    (reduce (fn [a [le re]]
                                                              (-> a
                                                                  (update :left conj (read-string le))
                                                                  (update :right conj (read-string re))))
                                                            {:left  []
                                                             :right []})
                                                    (vals)
                                                    (map frequencies))]
    (reduce-kv (fn [a value occur-left]
                 (let [occur-right (get second-frequencies value 0)]
                   (+ a (* value occur-left occur-right))))
               0
               first-frequencies)))

(comment
  (time (similarity-score input))

  (frequencies "emil tomas aref mattias novin anders")
  (frequencies [12 12 33 33 4444 44 444 4444 12 12])

  (->> {12 42
        44 100}
       (reduce-kv (fn [a k v]
                    (+ a k v))
                  0))

  (time (Thread/sleep 1000))

  (def numbers (range 10))
  (apply + numbers)
  (reduce + numbers)

  )
