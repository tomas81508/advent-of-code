(ns advent-of-code.dec-2022.day-13
  (:require [clojure.test :refer [deftest]]
            [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_13_input.txt")
                (clojure.string/split-lines)
                (remove (fn [l] (= l "")))
                (map read-string)
                (partition 2)))

(def test-input (->> "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"
                     (clojure.string/split-lines)
                     (remove (fn [l] (= l "")))
                     (map read-string)
                     (partition 2)))

(defn correct-order
  "Returns -1 if correct order, 0 if equal, 1 if incorrect order"
  {:test (fn []
           (is= (correct-order [1, 1, 3, 1, 1] [1, 1, 5, 1, 1])
                -1)
           (is= (correct-order [[1], [2, 3, 4]] [[1], 4])
                -1)
           (is= (correct-order [9] [[8, 7, 6]])
                1)
           (is= (correct-order [[4, 4], 4, 4] [[4, 4], 4, 4, 4])
                -1)
           (is= (correct-order [7, 7, 7, 7] [7, 7, 7])
                1)
           (is= (correct-order [] [3])
                -1)
           (is= (correct-order [[[]]] [[]])
                1)
           (is= (correct-order [1, [2, [3, [4, [5, 6, 7]]]], 8, 9] [1, [2, [3, [4, [5, 6, 0]]]], 8, 9])
                1))}
  [a b]
  (loop [[ca & ra] a
         [cb & rb] b]
    (cond
      ; both are collections
      (and (coll? ca) (coll? cb))
      (let [result (correct-order ca cb)]
        (if (zero? result)
          (recur ra rb)
          result))

      ; left is collection and right is not
      (and (coll? ca) (not (coll? cb)) cb)
      (let [result (correct-order ca [cb])]
        (if (zero? result)
          (recur ra rb)
          result))

      ; left is not collection and right is
      (and (not (coll? ca)) (coll? cb) ca)
      (let [result (correct-order [ca] cb)]
        (if (zero? result)
          (recur ra rb)
          result))

      ; left has values and right doesn't
      (and ca (not cb)) 1

      (and cb (not ca)) -1

      (and (not ca) (not cb)) 0

      (and (integer? ca) (integer? cb))
      (cond (< ca cb) -1
            (= ca cb) (recur ra rb)
            :else 1)

      :else
      -1)))

(defn solve-a
  [input]
  (->> input
       (map-indexed (fn [index pairs]
                      (when (neg? (apply correct-order pairs))
                        index)))
       (remove nil?)
       (map inc)
       (apply +)))

(deftest description-test-a
  (is= (solve-a test-input)
       13))

(defn solve-b
  [input]
  (->> (conj input [[[2]] [[6]]])
       (apply concat)
       (sort correct-order)
       (map-indexed (fn [index v] [index v]))
       (reduce (fn [a [index v]]
                 (if (or (= v [[2]]) (= v [[6]]))
                   (conj a (inc index))
                   a))
               [])
       (apply *)))

(deftest description-test-b
  (is= (solve-b test-input)
       140))


(comment

  (time (solve-a input))
  ; "Elapsed time: 0.576291 msecs"
  ; => 5013

  (time (solve-b input))
  ; "Elapsed time: 1.8127 msecs"
  ; => 3627

  )