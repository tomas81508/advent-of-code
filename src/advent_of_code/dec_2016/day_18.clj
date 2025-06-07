(ns advent-of-code.dec-2016.day-18
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def input "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^.......")

(def test-input "..^^.")

(defn trap? [c] (= c \^))
(defn safe? [c] (= c \.))

(defn count-safe-tiles-of-row
  {:test (fn []
           (is= (count-safe-tiles-of-row "..^^.") 3))}
  [row]
  (->> row
       (filter (fn [c] (= c \.)))
       (count)))

(defn next-row
  {:test (fn []
           (is= (next-row "..^^.") ".^^^^")
           (is= (next-row ".^^^^") "^^..^")
           )}
  [row]
  (->> (str "." row ".")
       (partition 3 1)
       (map (fn [[l c r]]
              (if (or (and (trap? l) (trap? c) (safe? r))
                      (and (trap? c) (trap? r) (safe? l))
                      (and (trap? l) (safe? c) (safe? r))
                      (and (trap? r) (safe? c) (safe? l)))
                \^
                \.)))
       (apply str)))

(defn count-safe-tiles
  [input rows]
  (->> (range (dec rows))
       (reduce (fn [a v]
                 (let [row (next-row (:row a))]
                   (-> (assoc a :row row)
                       (update :count + (count-safe-tiles-of-row row)))))
               {:row input
                :count (count-safe-tiles-of-row input)})))

(comment
  (time (count-safe-tiles input 40))
  ; "Elapsed time: 5.269917 msecs"
  ; 1913
  (time (count-safe-tiles input 400000))
  ; "Elapsed time: 12944.05225 msecs"
  ; 19993564
  )