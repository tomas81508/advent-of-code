(ns advent-of-code.dec-2020.day-05
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_05.txt")
       (clojure.string/split-lines)))

(defn find-row
  {:test (fn []
           (is= (find-row "BFFFBBFRRR") 70)
           (is= (find-row "FFFBBBFRRR") 14)
           (is= (find-row "BBFFBBFRLL") 102))}
  [string]
  (loop [[bf & bfs] (subs string 0 7)
         interval [0 127]]
    (let [half-span (quot (- (inc (second interval)) (first interval)) 2)]
      (if-not bf
        (first interval)
        (recur bfs
               (if (= bf \F)
                 (update interval 1 (fn [x] (- x half-span)))
                 (update interval 0 (fn [x] (+ x half-span)))))))))

(defn find-column
  {:test (fn []
           (is= (find-column "BFFFBBFRRR") 7)
           (is= (find-column "FFFBBBFRRR") 7)
           (is= (find-column "BBFFBBFRLL") 4))}
  [string]
  (loop [[lr & lrs] (subs string 7)
         interval [0 7]]
    (let [half-span (quot (- (inc (second interval)) (first interval)) 2)]
      (if-not lr
        (first interval)
        (recur lrs
               (if (= lr \L)
                 (update interval 1 (fn [x] (- x half-span)))
                 (update interval 0 (fn [x] (+ x half-span)))))))))

(defn find-seat-id
  {:test (fn []
           (is= (find-seat-id "BFFFBBFRRR") 567)
           (is= (find-seat-id "FFFBBBFRRR") 119)
           (is= (find-seat-id "BBFFBBFRLL") 820))}
  [string]
  (+ (* (find-row string) 8)
     (find-column string)))

(deftest puzzle-a
         (is= (->> (get-puzzle-input)
                   (map find-seat-id)
                   (apply max))
              947))

(deftest puzzle-b
         (is= (let [seat-ids (->> (get-puzzle-input)
                                  (map find-seat-id)
                                  (set))
                    min-seat-id (apply min seat-ids)
                    max-seat-id (apply max seat-ids)]
                (some (fn [id] (and (not (contains? seat-ids id))
                                    id))
                      (range min-seat-id (inc max-seat-id))))
              636))












