(ns advent-of-code.dec-2025.day-08-1
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
                #{{[162 817 812] #{}}
                  {[57 618 57] #{}}
                  {[906 360 560] #{}}
                  {[592 479 940] #{}}
                  {[352 342 300] #{}}
                  {[466 668 158] #{}}
                  {[542 29 236] #{}}
                  {[431 825 988] #{}}
                  {[739 650 466] #{}}
                  {[52 470 668] #{}}
                  {[216 146 977] #{}}
                  {[819 987 18] #{}}
                  {[117 168 530] #{}}
                  {[805 96 715] #{}}
                  {[346 949 466] #{}}
                  {[970 615 88] #{}}
                  {[941 993 340] #{}}
                  {[862 61 35] #{}}
                  {[984 92 344] #{}}
                  {[425 690 689] #{}}}))}
  [input]
  (->> input
       (map (fn [line]
              {(mapv read-string (re-seq #"\d+" line)) #{}}))
       (into #{})))

(declare get-distance-squared-between-junction-boxes)
(defn get-distance-squared-between-junction-boxes-raw
  {:test (fn []
           (is= (get-distance-squared-between-junction-boxes [1 1 1] [3 3 3]) 12))}
  [j1 j2]
  (->> (map - j1 j2)
       (map (fn [d] (* d d)))
       (reduce +)))
(def get-distance-squared-between-junction-boxes (memoize get-distance-squared-between-junction-boxes-raw))

(defn get-distance-and-closest-junction-boxes-between-circuits
  [c1 c2]
  (->> (for [j1 (keys c1)
             j2 (keys c2)
             :when (and (not (contains? (get c1 j1) j2))
                        (not= j1 j2))]
         [(get-distance-squared-between-junction-boxes j1 j2) j1 j2])
       (sort-by first)
       (first)))

(defn connect-closest-circuits
  {:test (fn []
           (is= (connect-closest-circuits (parse-input test-input))
                #{{[162 817 812] #{[425 690 689]}
                   [425 690 689] #{[162 817 812]}}
                  {[57 618 57] #{}}
                  {[906 360 560] #{}}
                  {[592 479 940] #{}}
                  {[352 342 300] #{}}
                  {[466 668 158] #{}}
                  {[542 29 236] #{}}
                  {[431 825 988] #{}}
                  {[739 650 466] #{}}
                  {[52 470 668] #{}}
                  {[216 146 977] #{}}
                  {[819 987 18] #{}}
                  {[117 168 530] #{}}
                  {[805 96 715] #{}}
                  {[346 949 466] #{}}
                  {[970 615 88] #{}}
                  {[941 993 340] #{}}
                  {[862 61 35] #{}}
                  {[984 92 344] #{}}}))}
  [circuits]
  (let [[c1 j1 c2 j2]
        (loop [[c & cs :as all-cs] circuits
               min-distance-squared ##Inf
               min-c1 nil
               min-j1 nil
               min-c2 nil
               min-j2 nil]
          (if (nil? c)
            [min-c1 min-j1 min-c2 min-j2]
            (let [[min-distance-squared min-c1 min-j1 min-c2 min-j2]
                  (loop [[cc & ccs] all-cs
                         min-distance-squared min-distance-squared
                         min-c1 min-c1
                         min-j1 min-j1
                         min-c2 min-c2
                         min-j2 min-j2]
                    (if (nil? cc)
                      [min-distance-squared min-c1 min-j1 min-c2 min-j2]
                      (let [[distance-squared j1 j2] (get-distance-and-closest-junction-boxes-between-circuits c cc)]
                        (if (and distance-squared (< distance-squared min-distance-squared))
                          (recur ccs distance-squared c j1 cc j2)
                          (recur ccs min-distance-squared min-c1 min-j1 min-c2 min-j2)))))]
              (recur cs min-distance-squared min-c1 min-j1 min-c2 min-j2))))]
    (-> circuits
        (disj c1 c2)
        (conj (-> (merge c1 c2)
                  (update j1 conj j2)
                  (update j2 conj j1))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 10) 40))}
  [input number-of-connections]
  (->> (range number-of-connections)
       (reduce (fn [circuits index]
                 (println index)
                 (connect-closest-circuits circuits))
               (parse-input input))
       (map count)
       (sort)
       (take-last 3)
       (apply *)))

(comment
  (time (part-1 input 1000))
  ; "Elapsed time: 313964.400167 msecs"
  ; => 75680

  )