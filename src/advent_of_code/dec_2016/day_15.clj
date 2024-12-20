(ns advent-of-code.dec-2016.day-15
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :refer [split-lines]]))

(def input (->> (slurp "src/advent_of_code/dec_2016/day_15_input.txt")
                (split-lines)))

(def test-input ["Disc #1 has 5 positions; at time=0, it is at position 4."
                 "Disc #2 has 2 positions; at time=0, it is at position 1."])

(defn create-disc-periods
  [input]
  (->> input
       (map (fn [row]
              (let [[n ps _ p] (map clojure.edn/read-string (re-seq #"\d+" row))]
                {:n (mod (- ps p n) ps)
                 :p ps})))))

(def test-discs (create-disc-periods test-input))
(def discs (create-disc-periods input))

(defn wait-value
  {:test (fn []
           (is= (wait-value test-discs) 5))}
  [discs]
  (loop [s 0]
    (if (every? (fn [{n :n p :p}] (zero? (rem (- s n) p))) discs)
      s
      (recur (inc s)))))

(comment
  (wait-value discs)
  )

(def input-2 (conj input "Disc #7 has 11 positions; at time=0, it is at position 0."))

(def discs-2 (create-disc-periods input-2))

(comment
  (wait-value discs-2)
  )




