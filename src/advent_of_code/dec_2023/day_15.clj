(ns advent-of-code.dec-2023.day-15
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]))

(def input (-> (slurp "src/advent_of_code/dec_2023/day_15_input.txt")))

(def test-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn handle-character
  {:test (fn []
           (is= (handle-character 0 \H) 200)
           (is= (handle-character 200 \A) 153)
           (is= (handle-character 153 \S) 172)
           (is= (handle-character 172 \H) 52))}
  [current character]
  (-> (+ current (int character))
      (* 17)
      (rem 256)))

(defn hash-algorithm
  {:test (fn []
           (is= (hash-algorithm "HASH") 52))}
  [s]
  (reduce handle-character 0 s))

(deftest test-puzzle-a
  (is= (->> (split test-input #",")
            (map hash-algorithm)
            (reduce +))
       1320))

(deftest puzzle-a
  (is= (time (->> (split input #",")
                  (map hash-algorithm)
                  (reduce +)))
       ; "Elapsed time: 6.877886 msecs"
       517551))

;; part 2

{0 [["rn" 1]]}

(defn create-boxes
  {:test (fn []
           (is= (create-boxes test-input)
                {0 [["rn" "1"] ["cm" "2"]]
                 1 []
                 3 [["ot" "7"] ["ab" "5"] ["pc" "6"]]}))}
  [input]
  (->> (split input #",")
       (reduce (fn [a s]
                 (let [[_ l o v] (re-find #"([a-z]+)(-|=)(\d*)" s)
                       h (hash-algorithm l)]
                   (case o
                     "=" (update a h (fn [box]
                                       (let [thing [l v]]
                                         (cond (nil? box) [thing]

                                               (some (fn [[bl _]] (= bl l)) box)
                                               (mapv (fn [x] (if (= (first x) l)
                                                               thing
                                                               x))
                                                     box)

                                               :else
                                               (conj box thing)))))
                     "-" (update a h (fn [box]
                                       (->> box
                                            (remove (fn [[bl _]] (= bl l)))
                                            (into [])))))))
               {})))

(defn focusing-power
  {:test (fn []
           (is= (focusing-power (create-boxes test-input))
                145))}
  [boxes]
  (->> boxes
       (reduce-kv (fn [a k vs]
                    (reduce-kv (fn [a index [_ v]]
                                 (+ a (* (inc k) (inc index) (read-string v))))
                               a
                               vs))
                  0)))

(deftest puzzle-b
  (is= (time (focusing-power (create-boxes input)))
       ; "Elapsed time: 37.274311 msecs"
       286097))

