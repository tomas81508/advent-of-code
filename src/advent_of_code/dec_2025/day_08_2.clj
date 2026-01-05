(ns advent-of-code.dec-2025.day-08-2
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.set :refer [union]]
            [clojure.math.combinatorics :refer [combinations]]))

; with Daniel Gullberg

(def input (->> (slurp "src/advent_of_code/dec_2025/day_08_input.txt")
                (string/split-lines)))

(def test-input ["162,817,812" "57,618,57" "906,360,560" "592,479,940" "352,342,300" "466,668,158" "542,29,236" "431,825,988" "739,650,466" "52,470,668" "216,146,977" "819,987,18" "117,168,530" "805,96,715" "346,949,466" "970,615,88" "941,993,340" "862,61,35" "984,92,344" "425,690,689"])

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input)
                #{#{[162 817 812]}
                  #{[57 618 57]}
                  #{[906 360 560]}
                  #{[592 479 940]}
                  #{[352 342 300]}
                  #{[466 668 158]}
                  #{[542 29 236]}
                  #{[431 825 988]}
                  #{[739 650 466]}
                  #{[52 470 668]}
                  #{[216 146 977]}
                  #{[819 987 18]}
                  #{[117 168 530]}
                  #{[805 96 715]}
                  #{[346 949 466]}
                  #{[970 615 88]}
                  #{[941 993 340]}
                  #{[862 61 35]}
                  #{[984 92 344]}
                  #{[425 690 689]}}))}
  [input]
  (->> input
       (map (fn [line] #{(mapv read-string (re-seq #"\d+" line))}))
       (into #{})))

(defn get-distance-squared-between-junction-boxes
  {:test (fn []
           (is= (get-distance-squared-between-junction-boxes [1 1 1] [3 3 3]) 12))}
  [j1 j2]
  (->> (map - j1 j2)
       (map (fn [d] (* d d)))
       (reduce +)))

(defn get-distance-squared-between-circuits
  {:test (fn []
           (is= (get-distance-squared-between-circuits #{[0 0 0] [1 1 1]} #{[3 3 3]})
                [12 [1 1 1] [3 3 3]]))}
  [c1 c2]
  (->> (for [j1 c1
             j2 c2]
         [(get-distance-squared-between-junction-boxes j1 j2) j1 j2])
       (sort-by first)
       (first)))

(defn connect-closest-circuits
  {:test (fn []
           (is= (connect-closest-circuits (parse-input test-input))
                [#{#{[162 817 812] [425 690 689]}
                   #{[57 618 57]}
                   #{[906 360 560]}
                   #{[592 479 940]}
                   #{[352 342 300]}
                   #{[466 668 158]}
                   #{[542 29 236]}
                   #{[431 825 988]}
                   #{[739 650 466]}
                   #{[52 470 668]}
                   #{[216 146 977]}
                   #{[819 987 18]}
                   #{[117 168 530]}
                   #{[805 96 715]}
                   #{[346 949 466]}
                   #{[970 615 88]}
                   #{[941 993 340]}
                   #{[862 61 35]}
                   #{[984 92 344]}}
                 [162 817 812]
                 [425 690 689]]))}
  [circuits]
  (let [[_ c1 c2 j1 j2] (->> (combinations circuits 2)
                             (reduce (fn [[min-distance-squared _ _ :as a] [c1 c2]]
                                       (let [[distance-squared j1 j2] (get-distance-squared-between-circuits c1 c2)]
                                         (if (< distance-squared min-distance-squared)
                                           [distance-squared c1 c2 j1 j2]
                                           a)))
                                     [##Inf nil nil]))]
    [(-> circuits
         (disj c1 c2)
         (conj (union c1 c2)))
     j1
     j2]))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 25272))}
  [input ]
  (loop [circuits (parse-input input)
         j1 nil
         j2 nil]
    (let [n (count circuits)]
      (println n)
      (if (= n 1)
        (* (first j1) (first j2))
        (let [[circuits j1 j2] (connect-closest-circuits circuits)]
          (recur circuits j1 j2))))))

(comment
  (time (part-2 input))
  ; "Elapsed time: 315080.201291 msecs"
  ; => 8995844880
  )
