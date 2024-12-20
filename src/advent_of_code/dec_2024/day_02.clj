(ns advent-of-code.dec-2024.day-02
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_02_input.txt")
                (clojure.string/split-lines)))

(def test-input ["7 6 4 2 1"
                 "1 2 7 8 9"
                 "9 7 6 2 1"
                 "1 3 2 4 5"
                 "8 6 4 4 1"
                 "1 3 6 7 9"])

(defn safe?
  {:test (fn []
           (is (safe? [7 6 4 2 1]))
           (is-not (safe? [1 2 7 8 9]))
           (is-not (safe? [9 7 6 2 1]))
           (is-not (safe? [1 3 2 4 5]))
           (is-not (safe? [8 6 4 4 1]))
           (is (safe? [1 3 6 7 9])))}
  [report]
  (and (or (apply < report)
           (apply > report))
       (->> report
            (partition 2 1)
            (map (fn [p] (abs (apply - p))))
            (some (fn [x] (> x 3)))
            (not))))


(comment
  (->> input
       (map (fn [row] (->> (re-seq #"\d+" row)
                           (map read-string))))
       (filter safe?)
       (count))
  )

; Part Two

(defn possible-reports
  {:test (fn []
           (is= (possible-reports [7 6 4 2 1])
                [[7 6 4 2 1] [6 4 2 1] [7 4 2 1] [7 6 2 1] [7 6 4 1] [7 6 4 2]]))}
  [report]
  (->> (range (count report))
       (reduce (fn [a i]
                 (conj a (let [[first-part second-part] (split-at i report)]
                           (concat first-part (drop 1 second-part)))))
               [report])))


(defn reactor-safe?
  {:test (fn []
           (is (reactor-safe? [7 6 4 2 1]))
           (is-not (reactor-safe? [1 2 7 8 9]))
           (is-not (reactor-safe? [9 7 6 2 1]))
           (is (reactor-safe? [1 3 2 4 5]))
           (is (reactor-safe? [8 6 4 4 1]))
           (is (reactor-safe? [1 3 6 7 9])))}
  [report]
  (->> (possible-reports report)
       (some safe?)))

(comment
  (time (->> input
             (map (fn [row] (->> (re-seq #"\d+" row)
                                 (map read-string))))
             (filter reactor-safe?)
             (count)))

  (->> [1 2 7 8 9 10 11]
       (partition 2 1))


  )


