(ns advent-of-code.dec-2023.day-05
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not is]]
            [clojure.string :refer [split]]))

; Together with Mattias Liljeström and Daniel Gullberg

(def almanac (-> (slurp "src/advent_of_code/dec_2023/day_05_input.txt")
                 (split #"\n\n")))

(def test-almanac (split "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"
                         #"\n\n"))

(defn source->destination-part-maker
  {:test (fn []
           (let [source->destination-part (source->destination-part-maker "52 50 2")]
             (is= (source->destination-part 51) 53)
             (is-not (source->destination-part 49))))}
  [mapping]
  (let [[dest src len] (->> (re-seq #"\d+" mapping)
                            (map read-string))]
    (fn [x]
      (when (<= src x (+ src (dec len)))
        (+ x (- dest src))))))

(defn source->destination-maker
  {:test (fn []
           (let [source->destination-part (source->destination-maker "seed-to-soil map:\n50 98 2\n52 50 48")]
             (is= (source->destination-part 99) 51)
             (is= (source->destination-part 75) 77)))}
  [almanac]
  (let [parts (split almanac #"\n")
        fns (mapv source->destination-part-maker (rest parts))]
    (fn [x]
      (some identity ((apply juxt (conj fns identity)) x)))))

(def test-start->end-fn (apply comp (reverse (map source->destination-maker (rest test-almanac)))))
(def start->end-fn (apply comp (reverse (map source->destination-maker (rest almanac)))))

(deftest start->end-fn-test
  (is= (test-start->end-fn 79) 82)
  (is= (test-start->end-fn 14) 43)
  (is= (test-start->end-fn 55) 86)
  (is= (test-start->end-fn 13) 35))

(defn calculate-min-seed
  [almanac]
  (let [seeds (->> (re-seq #"\d+" (first almanac))
                   (map read-string))]
    (->> seeds
         (map start->end-fn)
         (apply min))))

(deftest puzzle-a
  (is= (time (calculate-min-seed almanac))
       ; "Elapsed time: 0.445918 msecs"
       240320250))

;; Part b

(def test-almanac (split "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"
                         #"\n\n"))

(def seed-ranges (->> (first almanac)
                      (re-seq #"\d+")
                      (map read-string)
                      (partition 2)
                      (map (fn [[a b]] [a (+ a b)]))
                      (sort)))

(defn inverse-part-maker
  {:test (fn []
           (let [inverse-part (inverse-part-maker "52 50 2")]
             (is-not (inverse-part 54))
             (is= (inverse-part 53) 51)
             (is= (inverse-part 52) 50)
             (is-not (inverse-part 51))))}
  [mapping]
  (let [[dest src len] (->> (re-seq #"\d+" mapping)
                            (map read-string))]
    (fn [x]
      (when (<= dest x (+ dest (dec len)))
        (+ src (- x dest))))))

(defn inverse-maker
  {:test (fn []
           (let [inverse-fn (inverse-maker "seed-to-soil map:\n50 98 2\n52 50 48")]
             (is= (inverse-fn 51) 99)
             (is= (inverse-fn 77) 75)
             (is= (inverse-fn 15) 15)))}
  [almanac]
  (let [parts (split almanac #"\n")
        fns (mapv inverse-part-maker (rest parts))]
    (fn [x]
      (some identity ((apply juxt (conj fns identity)) x)))))

(defn end->start-maker
  [almanac]
  (apply comp (map inverse-maker (rest almanac))))

(def end->start-fn (end->start-maker almanac))
(def test-end->start-fn (end->start-maker test-almanac))

(deftest end->start-fn-test
  (is= (test-end->start-fn 82) 79)
  (is= (test-end->start-fn 43) 14)
  (is= (test-end->start-fn 86) 55)
  (is= (test-end->start-fn 35) 13))

(defn within-seed-ranges
  {:test (fn []
           (is (within-seed-ranges [[55 68] [79 93]] 63))
           (is-not (within-seed-ranges [[55 68] [79 93]] 72))
           (is (within-seed-ranges [[55 68] [79 93]] 85))
           (is-not (within-seed-ranges [[55 68] [79 93]] 99)))}
  [seed-ranges x]
  (loop [seed-ranges seed-ranges]
    (if (empty? seed-ranges)
      false
      (let [[[a b] & r] seed-ranges]
        (cond (< x a) false
              (<= a x b) true
              (> x b) (recur r))))))


; Not a good solution :(
(deftest puzzle-b
  (is= (time (loop [y 0]
               (let [x (end->start-fn y)]
                 (if (within-seed-ranges seed-ranges x)
                   (loop [y (- y 20000)]
                     (let [x (end->start-fn y)]
                       (if (within-seed-ranges seed-ranges x)
                         y
                         (recur (+ y 1)))))
                   (recur (+ y 20000))))))
       ; "Elapsed time: 26.930267 msecs"
       28580589))

