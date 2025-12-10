(ns advent-of-code.dec-2025.day-05
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.edn :as edn]))

; together with Daniel Gullberg and James Trunk

(def input (->> (slurp "src/advent_of_code/dec_2025/day_05_input.txt")
                (string/split-lines)))

(def test-input ["3-5"
                 "10-14"
                 "16-20"
                 "12-18"
                 ""
                 "1"
                 "5"
                 "8"
                 "11"
                 "17"
                 "32"])

(defn simplify-intervals
  {:test (fn []
           (is= (simplify-intervals [[3 5] [10 14] [16 20] [12 18]])
                [[3 5] [10 20]]))}
  [fresh-intervals]
  (->> fresh-intervals
       (sort-by first)
       (reduce (fn [acc [a b]]
                 (let [last-interval (last acc)]
                   (cond (empty? acc)
                         (conj acc [a b])

                         (<= a (second last-interval))
                         (update-in acc [(dec (count acc)) 1] max b)

                         :else
                         (conj acc [a b]))))
               [])))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:fresh-intervals [[3 5] [10 20]]
                 :ingredient-ids  [1 5 8 11 17 32]}))}
  [input]
  {:fresh-intervals (->> input
                         (take-while (fn [r] (not= r "")))
                         (map (fn [r] (map edn/read-string (re-seq #"\d+" r))))
                         (simplify-intervals))
   :ingredient-ids  (->> input
                         (filter (fn [r] (re-find #"^\d+$" r)))
                         (map edn/read-string))})

(defn fresh?
  {:test (fn []
           (is-not (fresh? [[3 5] [10 20]] 1))
           (is (fresh? [[3 5] [10 20]] 5))
           (is (fresh? [[3 5] [10 20]] 12)))}
  [fresh-intervals ingredient-id]
  (let [interval (->> fresh-intervals
                      (drop-while (fn [[_ b]] (< b ingredient-id)))
                      (first))]
    (and interval
         (>= ingredient-id (first interval)))))

(defn solve-1
  {:test (fn []
           (is= (solve-1 test-input) 3))}
  [input]
  (let [{intervals      :fresh-intervals
         ingredient-ids :ingredient-ids} (create-state input)]
    (->> ingredient-ids
         (filter (fn [id] (fresh? intervals id)))
         (count))))

(comment
  (time (solve-1 input))
  ; "Elapsed time: 13.063333 msecs"
  ; => 505
  )

(defn solve-2
  {:test (fn []
           (is= (solve-2 test-input) 14))}
  [input]
  (let [{intervals      :fresh-intervals
         ingredient-ids :ingredient-ids} (create-state input)]
    (->> intervals
         (map (fn [[a b]] (inc (- b a))))
         (reduce +))))

(comment
  (time (solve-2 input))
  ; "Elapsed time: 2.227 msecs"
  ; => 344423158480189
  )