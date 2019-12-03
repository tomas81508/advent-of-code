(ns advent-of-code.dec-2018.day-16
  (:require [ysera.test :refer [deftest is is=]]
            [clojure.string :as string]))

; Addition:
(defn addr
  {:test (fn []
           (is= (addr [3 2 1 1] [2 1 2])
                [3 2 3 1]))}
  [register [a b c]]
  (assoc register c (+ (nth register a)
                       (nth register b))))

(defn addi
  {:test (fn []
           (is= (addi [3 2 1 1] [2 1 2])
                [3 2 2 1]))}
  [register [a b c]]
  (assoc register c (+ (nth register a)
                       b)))

; Multiplication:
(defn mulr
  {:test (fn []
           (is= (mulr [3 2 1 1] [2 1 2])
                [3 2 2 1]))}
  [register [a b c]]
  (assoc register c (* (nth register a)
                       (nth register b))))

(defn muli
  {:test (fn []
           (is= (muli [3 2 1 1] [2 1 2])
                [3 2 1 1]))}
  [register [a b c]]
  (assoc register c (* (nth register a)
                       b)))

; Bitwise AND:
(defn banr
  {:test (fn []
           (is= (banr [3 2 1 1] [2 1 2])
                [3 2 0 1]))}
  [register [a b c]]
  (assoc register c (bit-and (nth register a)
                             (nth register b))))

(defn bani
  {:test (fn []
           (is= (bani [3 2 1 1] [2 1 2])
                [3 2 1 1]))}
  [register [a b c]]
  (assoc register c (bit-and (nth register a)
                             b)))

; Bitwise OR:
(defn borr
  {:test (fn []
           (is= (borr [3 2 1 1] [2 1 2])
                [3 2 3 1]))}
  [register [a b c]]
  (assoc register c (bit-or (nth register a)
                            (nth register b))))

(defn bori
  {:test (fn []
           (is= (bori [3 2 1 1] [2 1 2])
                [3 2 1 1]))}
  [register [a b c]]
  (assoc register c (bit-or (nth register a)
                            b)))

; Assignment:
(defn setr
  {:test (fn []
           (is= (setr [3 2 1 1] [2 1 2])
                [3 2 1 1]))}
  [register [a _ c]]
  (assoc register c (nth register a)))

(defn seti
  {:test (fn []
           (is= (seti [3 2 1 1] [2 1 2])
                [3 2 2 1]))}
  [register [a _ c]]
  (assoc register c a))

; Greater-than testing:
(defn gtir
  {:test (fn []
           (is= (gtir [3 2 1 1] [2 1 2])
                [3 2 0 1]))}
  [register [a b c]]
  (assoc register c (if (> a (nth register b)) 1 0)))

(defn gtri
  {:test (fn []
           (is= (gtri [3 2 1 1] [2 1 2])
                [3 2 0 1]))}
  [register [a b c]]
  (assoc register c (if (> (nth register a) b) 1 0)))

(defn gtrr
  {:test (fn []
           (is= (gtrr [3 2 1 1] [2 1 2])
                [3 2 0 1]))}
  [register [a b c]]
  (assoc register c (if (> (nth register a) (nth register b)) 1 0)))

; Equality testing:
(defn eqir
  {:test (fn []
           (is= (eqir [3 2 1 1] [2 1 2])
                [3 2 1 1]))}
  [register [a b c]]
  (assoc register c (if (= a (nth register b)) 1 0)))

(defn eqri
  {:test (fn []
           (is= (eqri [3 2 1 1] [2 1 2])
                [3 2 1 1]))}
  [register [a b c]]
  (assoc register c (if (= (nth register a) b) 1 0)))

(defn eqrr
  {:test (fn []
           (is= (eqrr [3 2 1 1] [2 1 2])
                [3 2 0 1]))}
  [register [a b c]]
  (assoc register c (if (= (nth register a) (nth register b)) 1 0)))

(def operations [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn three-opcodes?
  {:test (fn []
           (is (three-opcodes? [3 2 1 1] [9 2 1 2] [3 2 2 1])))}
  [before instruction after]
  (>= (reduce (fn [sum operation]
                (if (= (operation before (drop 1 instruction)) after)
                  (inc sum)
                  sum))
              0
              operations)
      3))

(defn get-sample
  {:test (fn []
           (is= (get-sample ["Before: [0, 1, 2, 2]" "3 2 2 1" "After:  [0, 2, 2, 2]"])
                [[0 1 2 2] [3 2 2 1] [0 2 2 2]]))}
  [[before-string instruction-string after-string]]
  (map (fn [tuple] (into [] (map read-string tuple)))
       [(-> (string/replace before-string #"Before: \[|,|\]" "")
            (string/split #" "))
        (string/split instruction-string #" ")
        (-> (string/replace after-string #"After:  \[|,|\]" "")
            (string/split #" "))]))

(deftest puzzle-a
         (is= (->> (-> (slurp "src/advent_of_code/dec_2018/day_16a.txt")
                       (clojure.string/split-lines))
                   (remove (fn [l] (= l "")))
                   (partition 3)
                   (map get-sample)
                   (filter (fn [sample] (apply three-opcodes? sample)))
                   (count))
              517))


(defn group-by-operation
  {:test (fn []
           (is= (group-by-operation [[[2 2 0 3] [2 0 3 2] [2 2 2 3]]
                                     [[0 1 2 1] [0 3 2 0] [1 1 2 1]]
                                     [[1 1 3 3] [1 1 0 1] [1 1 3 3]]
                                     [[0 1 2 2] [1 2 2 1] [0 2 2 2]]])
                {0 [[[0 1 2 1] [0 3 2 0] [1 1 2 1]]]
                 1 [[[1 1 3 3] [1 1 0 1] [1 1 3 3]]
                    [[0 1 2 2] [1 2 2 1] [0 2 2 2]]]
                 2 [[[2 2 0 3] [2 0 3 2] [2 2 2 3]]]}))}
  [samples]
  (reduce (fn [a [_ instruction _ :as sample]]
            (update a (first instruction) (fn [v]
                                            (if v (conj v sample) [sample]))))
          {}
          samples))


(def samples (->> (-> (slurp "src/advent_of_code/dec_2018/day_16a.txt")
                      (clojure.string/split-lines))
                  (remove (fn [l] (= l "")))
                  (partition 3)
                  (map get-sample)
                  (group-by-operation)))

(def number->operations
  (reduce-kv (fn [a k operation-samples]
               (reduce (fn [a operation]
                         (let [matching (->> (map (fn [[before instruction after]]
                                                    (= (operation before (drop 1 instruction)) after))
                                                  operation-samples)
                                             (some false?)
                                             (not))]
                           (if matching
                             (if (contains? a k)
                               (update a k conj operation)
                               (assoc a k [operation]))
                             a)))
                       a
                       operations))
             {}
             samples))

(def number->operation
  (loop [m number->operations
         i 100]
    (let [updated-m (reduce-kv (fn [m k v]
                                 (if (or (not (coll? v))
                                         (not= (count v) 1))
                                   m
                                   (let [operation (first v)
                                         m (assoc m k operation)]
                                     (reduce-kv (fn [m k v]
                                                  (if-not (coll? v)
                                                    m
                                                    (update m k (fn [vs]
                                                                  (remove (fn [x] (= x operation)) vs)))))
                                                m
                                                m))))
                               m
                               m)]
      (if (and (pos? i) (some coll? (vals updated-m)))
        (recur updated-m (dec i))
        updated-m))))

(defn solve-puzzle-b []
    (->> (-> (slurp "src/advent_of_code/dec_2018/day_16b.txt")
             (clojure.string/split-lines))
         (map (fn [l] (map read-string (string/split l #" "))))
         (reduce (fn [register [operation-number & instruction]]
                   ((number->operation operation-number) register instruction))
                 [0 0 0 0])))


