(ns advent-of-code.dec-2019.day-16
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_16.txt") $
        (seq $)
        (map str $)
        (map read-string $)))

(defn base-pattern
  {:test (fn []
           (is= (base-pattern 0 8)
                [1 0 -1 0 1 0 -1 0])
           (is= (base-pattern 1 8)
                [0 1 1 0 0 -1 -1 0])
           (is= (base-pattern 2 8)
                [0 0 1 1 1 0 0 0]))}
  [index size]
  (->> (cycle (concat (repeat (inc index) 0)
                      (repeat (inc index) 1)
                      (repeat (inc index) 0)
                      (repeat (inc index) -1)))
       (drop 1)
       (take size)))

(def base-pattern-memoized (memoize base-pattern))

(defn abs [x] (if (pos? x) x (- x)))

(defn phase
  {:test (fn []
           (is= (phase [1 2 3 4 5 6 7 8])
                [4 8 2 2 6 1 5 8])
           (is= (phase [4 8 2 2 6 1 5 8])
                [3 4 0 4 0 4 3 8])
           (is= (phase [3 4 0 4 0 4 3 8])
                [0 3 4 1 5 5 1 8])
           (is= (phase [0 3 4 1 5 5 1 8])
                [0 1 0 2 9 4 9 8]))}
  [input]
  (let [size (count input)]
    (reduce (fn [a index]
              (assoc a index
                       (as-> (map * input (base-pattern-memoized index size)) $
                             (apply + $)
                             (abs $)
                             (mod $ 10))))
            []
            (range size))))

(deftest larger-examples
  (is= (->> (reduce (fn [a _]
                      (phase a))
                    [8 0 8 7 1 2 2 4 5 8 5 9 1 4 5 4 6 6 1 9 0 8 3 2 1 8 6 4 5 5 9 5]
                    (range 100))
            (take 8))
       [2 4 1 7 6 1 7 6])
  (is= (->> (reduce (fn [a _]
                      (phase a))
                    [1 9 6 1 7 8 0 4 2 0 7 2 0 2 2 0 9 1 4 4 9 1 6 0 4 4 1 8 9 9 1 7]
                    (range 100))
            (take 8))
       [7 3 7 4 5 4 1 8])
  (is= (->> (reduce (fn [a _]
                      (phase a))
                    [6 9 3 1 7 1 6 3 4 9 2 9 4 8 6 0 6 3 3 5 9 9 5 9 2 4 3 1 9 8 7 3]
                    (range 100))
            (take 8))
       [5 2 4 3 2 1 3 3]))

(comment
  ; puzzle-a
  (time (->> (reduce (fn [a _] (phase a))
                     (get-puzzle-input)
                     (range 100))
             (take 8)))
  ; "Elapsed time: 4768.16106 msecs"
  [3 7 1 5 3 0 5 6])



;; PART b

(defn after-offset-phase
  {:test (fn []
           (is= (after-offset-phase [1 2 3 4 5 6 7 8] 4 1)
                [6 1 5 8])
           (is= (after-offset-phase [1 2 3 4 5 6 7 8] 6 2)
                [3 8])
           (is= (after-offset-phase [1 2 3 4 5 6 7 8] 5 3)
                [5 1 8])
           (is= (after-offset-phase [1 2 3 4 5 6 7 8] 4 4)
                [9 4 9 8]))}
  [input offset phases]
  {:pre [(>= offset (/ (count input) 2))]}
  (let [break (- (count input) offset)
        reversed-input (->> (reverse input)
                            (into []))]
    (->> (loop [phase-index 0
                input reversed-input]
           (if (= phase-index phases)
             input
             (recur (inc phase-index)
                    (loop [index 0
                           result []]
                      (if (>= index break)
                        result
                        (recur (inc index)
                               (conj result (mod (+ (get result (dec index) 0)
                                                    (get input index))
                                                 10))))))))
         (reverse))))

(comment
  (time (let [input (->> (get-puzzle-input)
                         (repeat 10000)
                         (flatten))
              offset (read-string (apply str (take 7 input)))]
          (->> (after-offset-phase input offset 100)
               (take 8))))
  ; "Elapsed time: 10925.463464 msecs"
  ; (6 0 5 9 2 1 9 9)
  )








