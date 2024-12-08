(ns advent-of-code.dec-2024.day-08
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_08_input.txt")
                (clojure.string/split-lines)))

(def test-input ["............"
                 "........0..."
                 ".....0......"
                 ".......0...."
                 "....0......."
                 "......A....."
                 "............"
                 "............"
                 "........A..."
                 ".........A.."
                 "............"
                 "............"])

{\0 [[8 1] [12 2]]
 \A [[]]}

(defn create-atlas
  [input]
  (->> input
       (reduce-kv (fn [a y row]
                    (->> (vec row)
                         (reduce-kv (fn [a x c]
                                      (if (= c \.)
                                        a
                                        (if-not (contains? a c)
                                          (assoc a c [[x y]])
                                          (update a c conj [x y]))))
                                    a)))
                  {})))

(def test-atlas (create-atlas test-input))
(def atlas (create-atlas input))

(defn vector-add
  {:test (fn []
           (is= (vector-add [1 3] [2 4])
                [3 7]))}
  [v1 v2]
  (map + v1 v2))

(defn vector-minus
  {:test (fn []
           (is= (vector-minus [3 3] [2 4])
                [1 -1]))}
  [v1 v2]
  (map - v1 v2))

(defn find-antinodes-by-pair
  {:test (fn []
           (is= (find-antinodes-by-pair [3 3] [5 5] 10)
                [[1 1] [7 7]])
           (is= (find-antinodes-by-pair [3 3] [5 5] 6)
                [[1 1]]))}
  [p1 p2 size]
  (let [diff (vector-minus p1 p2)]
    (->> [(vector-add p1 diff) (vector-minus p2 diff)]
         (remove (fn [[x y]] (or (neg? x)
                                 (neg? y)
                                 (>= x size)
                                 (>= y size)))))))

(defn find-antinodes
  [coll size]
  (let [cs (combinations coll 2)]
    (->> cs
         (map (fn [[p1 p2]] (find-antinodes-by-pair p1 p2 size)))
         (reduce concat))))

(defn find-unique-locations
  {:test (fn []
           (is= (find-unique-locations test-atlas 12)
                14))}
  [atlas size]
  (->> atlas
       (vals)
       (map (fn [points] (find-antinodes points size)))
       (reduce concat)
       (distinct)
       (count)))

(comment
  (time (find-unique-locations atlas 50))
  ; 285
  )

; part 2

(defn inside?
  {:test (fn []
           (is (inside? [2 3] 10))
           (is-not (inside? [-2 3] 10)))}
  [[x y] size]
  (and (>= x 0)
       (>= y 0)
       (< x size)
       (< y size)))

(defn find-antinodes-by-pair2
  {:test (fn []
           (is= (find-antinodes-by-pair2 [3 3] [5 5] 10)
                #{[1 1] [3 3] [5 5] [7 7] [9 9]})
           (is= (find-antinodes-by-pair2 [3 3] [5 5] 6)
                #{[1 1] [3 3] [5 5]}))}
  [p1 p2 size]
  (->> (let [diff (vector-minus p1 p2)]
         [(loop [result [p1]
                 current-point p1]
            (let [candidate (vector-add current-point diff)]
              (if (inside? candidate size)
                (recur (conj result candidate) candidate)
                result)))
          (loop [result [p2]
                 current-point p2]
            (let [candidate (vector-minus current-point diff)]
              (if (inside? candidate size)
                (recur (conj result candidate) candidate)
                result)))])
       (reduce concat)
       (into #{})))

(defn find-antinodes2
  [coll size]
  (let [cs (combinations coll 2)]
    (->> cs
         (map (fn [[p1 p2]] (find-antinodes-by-pair2 p1 p2 size)))
         (reduce concat))))

(defn find-unique-locations2
  {:test (fn []
           (is= (find-unique-locations2 test-atlas 12)
                34))}
  [atlas size]
  (->> atlas
       (vals)
       (map (fn [points] (find-antinodes2 points size)))
       (reduce concat)
       (distinct)
       (count)))

(comment
  (time (find-unique-locations2 atlas 50))
  ; 944
  )
