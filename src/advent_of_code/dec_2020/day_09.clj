(ns advent-of-code.dec-2020.day-09
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_09.txt")
       (clojure.string/split-lines)
       (mapv read-string)))

(def test-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])


(defn sum-of-numbers?
  {:test (fn []
           (is (sum-of-numbers? 40 [35 20 15 25 47]))
           (is (sum-of-numbers? 62 [20 15 25 47 40]))
           (is-not (sum-of-numbers? 127 [95 102 117 150 182])))}
  [n numbers]
  (-> (for [xi (range (count numbers))
            yi (range xi)
            :when (not= xi yi)
            :let [x (nth numbers xi)
                  y (nth numbers yi)]
            :when (= n (+ x y))]
        [x y])
      (empty?)
      (not)))


(defn find-first-error
  {:test (fn []
           (is= (find-first-error 5 test-input)
                127))}
  [preamble-amount numbers]
  (let [preamble-numbers (take preamble-amount numbers)
        numbers (drop preamble-amount numbers)]
    (loop [[n & numbers] numbers
           previous-numbers preamble-numbers]
      (if (sum-of-numbers? n previous-numbers)
        (recur numbers (concat (drop 1 previous-numbers) [n]))
        n))))

(deftest puzzle-a
  (is= (->> (get-puzzle-input)
            (find-first-error 25))
       1504371145))

(defn find-contiguous-set
  {:test (fn []
           (is= (find-contiguous-set 127 test-input)
                [15 25 47 40]))}
  [n numbers]
  (loop [start-index 0
         end-index 2]
    (let [sub-numbers (subvec numbers start-index end-index)
          sum (apply + sub-numbers)]
      (cond (> sum n)
            (recur (inc start-index) (+ 3 start-index))

            (= sum n)
            sub-numbers

            :else
            (recur start-index (inc end-index))))))

(defn add-smallest-and-largest-numbers
  [numbers]
  (+ (apply min numbers) (apply max numbers)))

(deftest puzzle-b
  (let [contiguous-set (time (find-contiguous-set 1504371145 (get-puzzle-input)))]
    ; "Elapsed time: 1325.877419 msecs"
    (is= contiguous-set
         [54418204
          76670764
          58849294
          82972768
          82657283
          89013309
          78479083
          88337414
          84689277
          87147239
          87462724
          91290880
          95930397
          128860283
          95091036
          125859569
          96641621])
    (is= (add-smallest-and-largest-numbers contiguous-set)
         183278487)))












