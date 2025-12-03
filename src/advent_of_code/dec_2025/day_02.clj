(ns advent-of-code.dec-2025.day-02
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

; together with Daniel Gullberg

(def input (-> (slurp "src/advent_of_code/dec_2025/day_02_input.txt")
               (string/split #",")))

(def test-input (-> "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
                    (string/split #",")))

(defn split-middle
  {:test (fn []
           (is= (split-middle "123123")
                [[\1 \2 \3] [\1 \2 \3]])
           (is= (split-middle "123")
                nil))}
  [s]
  (let [length (count s)]
    (when (even? length)
      (split-at (/ length 2) s))))

(defn invalid-id?
  {:test (fn []
           (is (invalid-id? "55"))
           (is (invalid-id? "6464"))
           (is (invalid-id? "123123"))
           (is-not (invalid-id? "101")))}
  [s]
  (let [parts (split-middle s)]
    (when parts
      (apply = parts))))

(defn invalid-ids-in-seq
  {:test (fn []
           (is= (invalid-ids-in-seq 11 22)
                [11 22])
           (is= (invalid-ids-in-seq 95 115)
                [99]))}
  ([a b] (invalid-ids-in-seq a b invalid-id?))
  ([a b invalid-id-fn]
   (->> (range a (inc b))
        (filter (fn [n] (when (invalid-id-fn (str n)) n))))))

(defn solve-puzzle-1
  {:test (fn []
           (is= (solve-puzzle-1 test-input) 1227775554))}
  [input]
  (->> input
       (mapcat (fn [s]
                 (let [interval (->> (re-seq #"\d+" s)
                                     (map read-string))]
                   (apply invalid-ids-in-seq interval))))
       (reduce +)))

(comment
  (time (solve-puzzle-1 input))
  ; "Elapsed time: 424.293208 msecs"
  ; => 54641809925
  )

; part 2

(defn get-dividers-raw
  {:test (fn []
           (is= (get-dividers-raw 10) [2 5])
           (is= (get-dividers-raw 7) []))}
  [n]
  (->> (range 2 n)
       (filter (fn [m] (zero? (rem n m))))))

(def get-dividers (memoize get-dividers-raw))

(defn invalid-id-2?
  {:test (fn []
           (is-not (invalid-id-2? "5"))
           (is (invalid-id-2? "55"))
           (is (invalid-id-2? "123123123"))
           (is (invalid-id-2? "1212121212"))
           (is-not (invalid-id-2? "101")))}
  [s]
  (let [length (count s)
        dividers (get-dividers length)]
    (when (> length 1)
      (->> (cons 1 dividers)
           (some (fn [d] (->> (seq s)
                              (partition d)
                              (apply =))))))))

(defn solve-puzzle-2
  {:test (fn []
           (is= (solve-puzzle-2 test-input) 4174379265))}
  [input]
  (->> input
       (mapcat (fn [s]
                 (let [[a b] (->> (re-seq #"\d+" s)
                                  (map read-string))]
                   (invalid-ids-in-seq a b invalid-id-2?))))
       (reduce +)))

(comment
  (time (solve-puzzle-2 input))
  ; "Elapsed time: 3249.461416 msecs"
  ; => 73694270688
  )












