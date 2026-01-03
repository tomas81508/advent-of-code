(ns advent-of-code.dec-2025.day-07
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def input (->> (slurp "src/advent_of_code/dec_2025/day_07_input.txt")
                (string/split-lines)))

(def test-input [".......S......."
                 "..............."
                 ".......^......."
                 "..............."
                 "......^.^......"
                 "..............."
                 ".....^.^.^....."
                 "..............."
                 "....^.^...^...."
                 "..............."
                 "...^.^...^.^..."
                 "..............."
                 "..^...^.....^.."
                 "..............."
                 ".^.^.^.^.^...^."
                 "..............."])

(defn move-down-a-step
  {:test (fn []
           (is= (move-down-a-step test-input 0 #{7}) [#{7} 0])
           (is= (move-down-a-step test-input 1 #{7}) [#{6 8} 1])
           (is= (move-down-a-step test-input 2 #{6 8}) [#{6 8} 0])
           (is= (move-down-a-step test-input 3 #{6 8}) [#{5 7 9} 2]))}
  [input index positions]
  (->> positions
       (reduce (fn [a v]
                 (let [c (get-in input [(inc index) v])]
                   (if (= c \.)
                     (update a 0 conj v)
                     (-> a
                         (update 0 conj (dec v) (inc v))
                         (update 1 inc)))))
               [#{} 0])))

(defn solve-1
  {:test (fn []
           (is= (solve-1 test-input) 21))}
  [input]
  (let [start-position (->> (first input)
                            (map-indexed (fn [index item] [index item]))
                            (some (fn [[index item]] (when (= item \S) index))))]
    (->> (range (dec (count input)))
         (reduce (fn [[positions splits] v]
                   (let [[positions new-splits] (move-down-a-step input v positions)]
                     [positions (+ splits new-splits)]))
                 [#{start-position} 0])
         (second))))

(comment
  (time (solve-1 input))
  ; "Elapsed time: 16.121625 msecs"
  ; => 1592
  )

(defn move-down-a-step-2
  {:test (fn []
           (is= (move-down-a-step-2 test-input 0 {7 1}) {7 1})
           (is= (move-down-a-step-2 test-input 1 {7 1}) {6 1 8 1})
           (is= (move-down-a-step-2 test-input 2 {6 1 8 1}) {6 1 8 1})
           (is= (move-down-a-step-2 test-input 3 {6 1 8 1}) {5 1 7 2 9 1}))}
  [input index positions]
  (->> positions
       (reduce-kv (fn [a p n]
                    (let [c (get-in input [(inc index) p])]
                      (if (= c \.)
                        (update a p (fn [m] (+ n (or m 0))))
                        (-> a
                            (update (dec p) (fn [m] (+ n (or m 0))))
                            (update (inc p) (fn [m] (+ n (or m 0))))))))
                  {})))

(defn solve-2
  {:test (fn []
           (is= (solve-2 test-input) 40))}
  [input]
  (let [start-position (->> (first input)
                            (map-indexed (fn [index item] [index item]))
                            (some (fn [[index item]] (when (= item \S) index))))]
    (->> (range (dec (count input)))
         (reduce (fn [positions v]
                   (move-down-a-step-2 input v positions))
                 {start-position 1})
         (vals)
         (reduce + 0))))

(comment
  (time (solve-2 input))
  ; "Elapsed time: 4.946958 msecs"
  ; => 17921968177009
  )

