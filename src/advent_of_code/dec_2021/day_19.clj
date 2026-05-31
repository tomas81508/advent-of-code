(ns advent-of-code.dec-2021.day-19
  (:require [advent-of-code.test :refer [is=]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.set]))

(def input (slurp "src/advent_of_code/dec_2021/day_19_input.txt"))

(def test-input "--- scanner 0 ---\n0,2\n4,1\n3,3\n\n--- scanner 1 ---\n-1,-1\n-5,0\n-2,1")

(def test-input-2 (slurp "src/advent_of_code/dec_2021/day_19_test_input.txt"))

(defn create-scanners
  {:test (fn []
           (is= (create-scanners test-input)
                {0 #{[0 2] [4 1] [3 3]}
                 1 #{[-1 -1] [-5 0] [-2 1]}}))}
  [input]
  (->> (string/split input #"\n\n")
       (map (fn [scanner-data]
              (->> scanner-data
                   (string/split-lines)
                   (drop 1)
                   (map (fn [ns] (->> (string/split ns #",")
                                      (map edn/read-string)))))))
       (vec)
       (reduce-kv (fn [a index coordinates]
                    (assoc a index (set coordinates)))
                  {})))

(def test-scanners (create-scanners test-input))
(def test-scanners-2 (create-scanners test-input-2))
(def scanners (create-scanners input))

; basic rotations
(defn rotate-x [[x y z]] [x (- z) y])
(defn rotate-y [[x y z]] [z y (- x)])
(defn rotate-z [[x y z]] [(- y) x z])

; all rotations
(def orientations
  [(fn [[x y z]] [x y z])
   (fn [[x y z]] [x (- z) y])
   (fn [[x y z]] [x (- y) (- z)])
   (fn [[x y z]] [x z (- y)])

   (fn [[x y z]] [(- x) (- y) z])
   (fn [[x y z]] [(- x) z y])
   (fn [[x y z]] [(- x) y (- z)])
   (fn [[x y z]] [(- x) (- z) (- y)])

   (fn [[x y z]] [(- y) x z])
   (fn [[x y z]] [z x y])
   (fn [[x y z]] [y x (- z)])
   (fn [[x y z]] [(- z) x (- y)])

   (fn [[x y z]] [y (- x) z])
   (fn [[x y z]] [z (- x) (- y)])
   (fn [[x y z]] [(- y) (- x) (- z)])
   (fn [[x y z]] [(- z) (- x) y])

   (fn [[x y z]] [(- z) y x])
   (fn [[x y z]] [y z x])
   (fn [[x y z]] [z (- y) x])
   (fn [[x y z]] [(- y) (- z) x])

   (fn [[x y z]] [z y (- x)])
   (fn [[x y z]] [y (- z) (- x)])
   (fn [[x y z]] [(- z) (- y) (- x)])
   (fn [[x y z]] [(- y) z (- x)])])

;(defn create-all-beacons-orientations
;  {:test (fn []
;           (is= (count (create-all-beacons-orientations (get test-scanners-2 0)))
;                24))}
;  [beacons]
;  (->> orientations
;       (reduce-kv (fn [a orientation] (conj a (map orientation beacons)))
;               [])))

(defn translate
  {:test (fn []
           (is= (translate (get test-scanners 1) [5 2])
                (get test-scanners 0)))}
  [beacons translation]
  (->> beacons
       (map (fn [b2] (map + b2 translation)))
       (into #{})))

(declare get-transform-if-intersect)

(defn get-transform-if-intersect-raw
  {:test (fn []
           (is= (get-transform-if-intersect-raw (get test-scanners-2 0) (get test-scanners-2 1))
                {:orientation 6, :translation [68 -1246 -43]}))}
  [beacons1 beacons2]
  (->> orientations
       (map-indexed (fn [index orientation] [index orientation]))
       (some (fn [[index orientation]]
               (let [beacons2 (map orientation beacons2)]
                 (->> beacons2
                      (some (fn [b2]
                              (->> beacons1
                                   (some (fn [b1]
                                           (let [translation (map - b1 b2)
                                                 beacons2 (translate beacons2 translation)
                                                 hits (count (clojure.set/intersection beacons1 beacons2))]
                                             (when (> hits 11)
                                               {:orientation index
                                                :translation translation})))))))))))))

(def get-transform-if-intersect (memoize get-transform-if-intersect-raw))

(defn transform-beacons
  [beacons {orientation :orientation translation :translation}]
  (let [orientation-fn (get orientations orientation)]
    (-> (map orientation-fn beacons)
        (translate translation))))

(defn unify-beacons
  [m]
  (->> (map :beacons m)
       (reduce (fn [a bs] (clojure.set/union a bs)) #{})))

(defn construct-map
  {:test (fn []
           (is= (count (unify-beacons (construct-map test-scanners-2)))
                79))}
  [scanners]
  (loop [result [{:scanner [0 0 0]
                  :beacons (get scanners 0)}]
         unknown-beacons-list (set (drop 1 (vals scanners)))]
    (println (count result) (count unknown-beacons-list))
    (if (empty? unknown-beacons-list)
      result
      (let [{unknown-beacons :unknown-beacons
             transformation  :transformation} (->> unknown-beacons-list
                                                   (some (fn [unknown-beacons]
                                                           (->> result
                                                                (map :beacons)
                                                                (some (fn [beacons]
                                                                        (when-let [transformation (get-transform-if-intersect beacons unknown-beacons)]
                                                                          {:unknown-beacons unknown-beacons
                                                                           :transformation  transformation})))))))]
        (recur (conj result {:scanner transformation
                             :beacons (transform-beacons unknown-beacons transformation)})
               (disj unknown-beacons-list unknown-beacons))))))


(comment
  (def total-map (construct-map scanners))
  (count (unify-beacons (construct-map scanners)))
  (as-> total-map $
        (map (comp :translation :scanner) $)
        (combinations $ 2)
        (map (fn [[s1 s2]]
               (->> (map - s1 s2)
                    (map abs)
                    (reduce +)))
             $)
        (reduce max $))

  )




