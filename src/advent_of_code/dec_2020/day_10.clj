(ns advent-of-code.dec-2020.day-10
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_10.txt")
       (clojure.string/split-lines)
       (mapv read-string)))

(def test-input [16 10 15 5 1 11 7 19 6 12 4])

(def test-input-2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(defn map-2
  [f coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (when (second s)
        (cons (f (first s) (second s)) (map-2 f (drop 1 s)))))))

(defn find-jolts-differencies
  {:test (fn []
           (is= (find-jolts-differencies test-input)
                {1 7
                 3 5})
           (is= (find-jolts-differencies test-input-2)
                {1 22
                 3 10}))}
  [adapter-jolts]
  (->> (conj adapter-jolts 0 (+ 3 (apply max adapter-jolts)))
       (sort)
       (map-2 (fn [x1 x2] (- x2 x1)))
       (frequencies)))

(deftest puzzle-a
  (is= (->> (find-jolts-differencies (get-puzzle-input))
            (vals)
            (apply *))
       2040))

(declare find-number-of-adapter-arrangements-up-to-memoized)

(defn find-number-of-adapter-arrangements-up-to
  {:test (fn []
           (let [jolts (sort (conj test-input 0 (+ 3 (apply max test-input))))]
             (is= (find-number-of-adapter-arrangements-up-to jolts 1) 1)
             (is= (find-number-of-adapter-arrangements-up-to jolts 4) 1)
             (is= (find-number-of-adapter-arrangements-up-to jolts 5) 1)
             (is= (find-number-of-adapter-arrangements-up-to jolts 6) 2)
             (is= (find-number-of-adapter-arrangements-up-to jolts 7) 4)
             (is= (find-number-of-adapter-arrangements-up-to jolts 10) 4)
             (is= (find-number-of-adapter-arrangements-up-to jolts 11) 4)
             (is= (find-number-of-adapter-arrangements-up-to jolts 12) 8)
             (is= (find-number-of-adapter-arrangements-up-to jolts 15) 8)
             (is= (find-number-of-adapter-arrangements-up-to jolts 16) 8)
             (is= (find-number-of-adapter-arrangements-up-to jolts 19) 8)
             (is= (find-number-of-adapter-arrangements-up-to jolts 22) 8))
           (let [jolts (sort (conj test-input-2 0 (+ 3 (apply max test-input-2))))
                 max-jolt (last jolts)]
             (is= (find-number-of-adapter-arrangements-up-to jolts max-jolt)
                  19208)))}
  [adapter-jolts wanted-jolt]
  (let [existing-jolt-adapters (->> adapter-jolts
                                    (filter (fn [aj] (< (- wanted-jolt 4) aj wanted-jolt))))]
    (if (empty? existing-jolt-adapters)
      1
      (reduce (fn [a jolt-adapter]
                (+ a
                   (find-number-of-adapter-arrangements-up-to-memoized adapter-jolts jolt-adapter)))
              0
              existing-jolt-adapters))))

(def find-number-of-adapter-arrangements-up-to-memoized (memoize find-number-of-adapter-arrangements-up-to))

(deftest puzzle-b
  (is= (time (let [input (get-puzzle-input)
                   max-jolt (+ 3 (apply max input))]
               (as-> (conj input 0 max-jolt) $
                     (sort $)
                     (find-number-of-adapter-arrangements-up-to $ max-jolt))))
       ;"Elapsed time: 0.7467 msecs"
       28346956187648))

















