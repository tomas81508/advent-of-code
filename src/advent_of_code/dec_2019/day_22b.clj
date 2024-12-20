(ns advent-of-code.dec-2019.day-22b
  (:require [advent-of-code.test :refer [is is= is-not]]
            [clojure.test :refer [deftest]]))


(defn new-stack-index
  {:test (fn []
           (is= (new-stack-index 10 1) 8))}
  [size index]
  (dec (- size index)))

(defn inverse-new-stack-index
  {:test (fn []
           (is= (inverse-new-stack-index 10 1) 8))}
  [size index]
  (dec (- size index)))

(defn cut-N-index
  {:test (fn []
           (is= (cut-N-index 10 3 1) 8)
           (is= (cut-N-index 10 3 2) 9)
           (is= (cut-N-index 10 3 3) 0)
           (is= (cut-N-index 10 3 4) 1)
           (is= (cut-N-index 10 3 5) 2)
           (is= (cut-N-index 10 -2 0) 2)
           (is= (cut-N-index 10 -4 2) 6)
           (is= (cut-N-index 10 -4 7) 1))}
  [size n index]
  (if (pos? n)
    (if (< index n) (+ (- size n) index) (- index n))
    (if (< index (+ size n))
      (- index n)
      (- index (+ size n)))))

(defn inverse-cut-N-index
  {:test (fn []
           (is= (inverse-cut-N-index 10 3 8) 1)
           (is= (inverse-cut-N-index 10 3 9) 2)
           (is= (inverse-cut-N-index 10 3 2) 5)
           (is= (inverse-cut-N-index 10 3 1) 4)
           (is= (inverse-cut-N-index 10 3 0) 3)
           (is= (inverse-cut-N-index 10 -2 2) 0)
           (is= (inverse-cut-N-index 10 -4 6) 2)
           (is= (inverse-cut-N-index 10 -4 1) 7))}
  [size n index]
  (if (pos? n)
    (if (>= (- size n) index)
      (+ index n)
      (- index (- size n)))
    (if (not (neg? (+ index n)))
      (+ index n)
      (+ index size n))))

(defn increment-N-index
  {:test (fn []
           (is= (increment-N-index 10 3 2) 6)
           (is= (increment-N-index 10 3 5) 5)
           (is= (increment-N-index 10 3 9) 7))}
  [size n index]
  (mod (* n index) size))

(defn inverse-increment-N-index
  {:test (fn []
           (is= (inverse-increment-N-index 10 3 0) 0)
           (is= (inverse-increment-N-index 10 3 3) 1)
           (is= (inverse-increment-N-index 10 3 6) 2)
           (is= (inverse-increment-N-index 10 3 5) 5)
           (is= (inverse-increment-N-index 10 3 7) 9))}
  [size n index]
  (loop [k 0]
    (let [result (/ (+ index (* k size)) n)]
      (if (integer? result)
        result
        (recur (inc k))))))

(deftest test-1
         (is= (->> (increment-N-index 10 7 3)
                   (new-stack-index 10)
                   (new-stack-index 10))
              1))

(deftest test-1-inverse
         (is= (->> (inverse-new-stack-index 10 1)
                   (inverse-new-stack-index 10)
                   (inverse-increment-N-index 10 7))
              3))

(deftest test-2
         (is= (->> (cut-N-index 10 6 9)
                   (increment-N-index 10 7)
                   (new-stack-index 10))
              8))

(deftest test-2-inverse
         (is= (->> (inverse-new-stack-index 10 8)
                   (inverse-increment-N-index 10 7)
                   (inverse-cut-N-index 10 6))
              9))

(deftest test-3
         (is= (->> (increment-N-index 10 7 8)
                   (increment-N-index 10 9)
                   (cut-N-index 10 -2))
              6))

(deftest test-3-inverse
         (is= (->> (inverse-cut-N-index 10 -2 6)
                   (inverse-increment-N-index 10 9)
                   (inverse-increment-N-index 10 7)
                   )
              8))

(deftest test-4
         (is= (->> (new-stack-index 10 9)
                   (cut-N-index 10 -2)
                   (increment-N-index 10 7)
                   (cut-N-index 10 8)
                   (cut-N-index 10 -4)
                   (increment-N-index 10 7)
                   (cut-N-index 10 3)
                   (increment-N-index 10 9)
                   (increment-N-index 10 3)
                   (cut-N-index 10 -1))
              0))

(deftest test-4-inverse
         (is= (->> (inverse-cut-N-index 10 -1 0)
                   (inverse-increment-N-index 10 3)
                   (inverse-increment-N-index 10 9)
                   (inverse-cut-N-index 10 3)
                   (inverse-increment-N-index 10 7)
                   (inverse-cut-N-index 10 -4)
                   (inverse-cut-N-index 10 8)
                   (inverse-increment-N-index 10 7)
                   (inverse-cut-N-index 10 -2)
                   (inverse-new-stack-index 10))
              9))


(def puzzle-input (->> (slurp "src/advent_of_code/dec_2019/day_22.txt")
                       (clojure.string/split-lines)))

(defn solver-a
  {:test (fn []
           (is= (solver-a 10
                          2
                          ["cut 6"
                           "deal with increment 7"
                           "deal into new stack"])
                7))}
  [size index instructions]
  (let [cut-N-pattern (re-pattern "cut ([-]*\\d+)")
        increment-N-pattern (re-pattern "deal with increment (\\d+)")]
    (reduce (fn [index instruction]
              (cond (clojure.string/starts-with? instruction "cut")
                    (let [[_ n] (re-matches cut-N-pattern instruction)]
                      (cut-N-index size (read-string n) index))

                    (clojure.string/starts-with? instruction "deal with increment")
                    (let [[_ n] (re-matches increment-N-pattern instruction)]
                      (increment-N-index size (read-string n) index))

                    (= instruction "deal into new stack")
                    (new-stack-index size index)))
            index
            instructions)))

(defn instruction->inverse-operation
  [size instruction]
  (let [cut-N-pattern (re-pattern "cut ([-]*\\d+)")
        increment-N-pattern (re-pattern "deal with increment (\\d+)")]
    (cond (clojure.string/starts-with? instruction "cut")
          (let [[_ n] (re-matches cut-N-pattern instruction)]
            (partial inverse-cut-N-index size (read-string n)))

          (clojure.string/starts-with? instruction "deal with increment")
          (let [[_ n] (re-matches increment-N-pattern instruction)]
            (partial inverse-increment-N-index size (read-string n)))

          (= instruction "deal into new stack")
          (partial inverse-new-stack-index size))))

(defn inverse-solver
  {:test (fn []
           (is= (inverse-solver 10
                                7
                                ["cut 6"
                                 "deal with increment 7"
                                 "deal into new stack"]
                                1)
                2))}
  [size index instructions repetitions]
  (let [inverted-operations (->> (reverse instructions)
                                 (map (fn [i] (instruction->inverse-operation size i))))]
    (loop [iterations 0
           index index]
      (when (zero? (mod iterations 100000))
        (println iterations))
      (if (= iterations repetitions)
        index
        (recur (inc iterations)
               (reduce (fn [index inverted-operation]
                         (inverted-operation index))
                       index
                       inverted-operations))))))

(deftest puzzle-a
         (is= (time (solver-a 10007 2019 puzzle-input))
              ; "Elapsed time: 0.510461 msecs"
              6417))

(deftest puzzle-a-inverse
         (is= (inverse-solver 10007 6417 puzzle-input 1)
              2019))

(comment
  (time (inverse-solver 119315717514047 2020 puzzle-input 101741582076661)))



