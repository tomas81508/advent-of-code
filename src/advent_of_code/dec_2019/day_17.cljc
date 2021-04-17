(ns advent-of-code.dec-2019.day-17
  (:require [advent-of-code.dec-2019.day-09 :refer [run
                                                    run-instruction
                                                    create-program]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_17.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (into [] $)))

(def int-code (get-puzzle-input))

(def program (create-program int-code []))

(def result (run program))

(defn print-map
  [result]
  (->> (:outputs result)
       (reduce (fn [a v]
                 (str a
                      (case v
                        35 "#"
                        46 "."
                        10 "\n"
                        94 "^")))
               "")))

(print-map result)

(defn get-path
  [result]
  (->> (:outputs result)
       (partition 50)
       (map drop-last)
       (map-indexed (fn [y line] [y line]))
       (reduce (fn [a [y line]]
                 (reduce (fn [a [x n]]
                           (if (= n 35)
                             (conj a [x y])
                             a))
                         a
                         (->> line
                              (map-indexed (fn [x n] [x n])))))
               #{})))

(def path (get-path result))

(defn get-intersections
  [path]
  (let [directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (reduce (fn [a v]
              (if (->> directions
                       (map (fn [d] (map + d v)))
                       (every? (partial contains? path)))
                (conj a v)
                a))
            #{}
            path)))

(def intersections (get-intersections path))

(def puzzle-a (->> intersections
                   (map (fn [[x y]] (* x y)))
                   (apply +)))

;; PART TWO

(def trip ["R" "12" "R" "4" "R" "10" "R" "12" "R" "6" "L" "8" "R" "10" "R" "12" "R" "4" "R" "10"
           "R" "12" "L" "8" "R" "4" "R" "4" "R" "6" "R" "12" "R" "4" "R" "10" "R" "12" "R" "6"
           "L" "8" "R" "10" "L" "8" "R" "4" "R" "4" "R" "6" "R" "12" "R" "4" "R" "10" "R" "12"
           "R" "6" "L" "8" "R" "10" "L" "8" "R" "4" "R" "4" "R" "6"])

(def A ["R" "12" "R" "4" "R" "10" "R" "12"])

(def B ["R" "6" "L" "8" "R" "10"])

(def C ["L" "8" "R" "4" "R" "4" "R" "6"])

(def MAIN [A B A C A B C A B C])

(= (flatten MAIN) trip)

(def clean-robot-program (update program :int-code (fn [ints] (assoc ints 0 2))))

(def string->ascii {\0 48 \1 49 \2 50 \4 52 \6 54 \8 56 \R 82 \L 76 \, 44 \newline 10 \A 65 \B 66 \C 67 \n 110 \y 121})

(def a (str (clojure.string/join "," A) "\n"))
(def b (str (clojure.string/join "," B) "\n"))
(def c (str (clojure.string/join "," C) "\n"))
(def main "A,B,A,C,A,B,C,A,B,C\n")

(def inputs (->> (str main a b c "n\n")
                 (map string->ascii)))

(reduce (fn [a input]
          (run a [input]))
        clean-robot-program
        inputs)