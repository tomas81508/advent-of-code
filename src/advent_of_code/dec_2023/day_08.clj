(ns advent-of-code.dec-2023.day-08
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]
            [clojure.string :refer [ends-with? split-lines]]
            [clojure.math]))

(def input (-> (slurp "src/advent_of_code/dec_2023/day_08_input.txt")
               (split-lines)))

(defn create-the-map
  [input]
  {:directions (first input)
   :map        (->> (drop 2 input)
                    (reduce (fn [a r]
                              (let [[from left right] (re-seq #"[A-Z]+" r)]
                                (assoc a from [left right])))
                            {}))})

(def the-map (create-the-map input))

(def test-map-1 {:directions "RL"
                 :map        {"AAA" ["BBB" "CCC"]
                              "BBB" ["DDD" "EEE"]
                              "CCC" ["ZZZ" "GGG"]}})

(def test-map-2 {:directions "LLR"
                 :map        {"AAA" ["BBB" "BBB"]
                              "BBB" ["AAA" "ZZZ"]
                              "ZZZ" ["ZZZ" "ZZZ"]}})

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-map-1 0 "AAA")
                [1 "CCC"])
           (is= (->> [0 "AAA"]
                     (apply walk-a-step test-map-1)
                     (apply walk-a-step test-map-1))
                [0 "ZZZ"])
           (is= (->> [2 "AAA"]
                     (apply walk-a-step test-map-2))
                [0 "BBB"]))}
  [the-map index position]
  (let [directions (:directions the-map)
        direction (nth directions index)]
    [(rem (inc index) (count directions))
     (get-in the-map [:map position (if (= direction \L) 0 1)])]))

(defn walk-to-end
  {:test (fn []
           (is= (walk-to-end test-map-1) [2 "ZZZ"])
           (is= (walk-to-end test-map-2) [6 "ZZZ"]))}
  ([the-map] (walk-to-end the-map "AAA" (fn [p] (= p "ZZZ"))))
  ([the-map starting-position end-position-pred]
   (loop [[index position] [0 starting-position]
          steps 0]
     (if (and (end-position-pred position)
              (pos? steps))
       [steps position]
       (recur (walk-a-step the-map index position) (inc steps))))))

(deftest puzzle-a
  (is= (time (first (walk-to-end the-map)))
       ; "Elapsed time: 21.94364 msecs"
       18157))

(def test-map-3 {:directions "LR"
                 :map        {"11A" ["11B" "XXX"]
                              "11B" ["XXX" "11Z"]
                              "11Z" ["11B" "XXX"]
                              "22A" ["22B" "XXX"]
                              "22B" ["22C" "22C"]
                              "22C" ["22Z" "22Z"]
                              "22Z" ["22B" "22B"]
                              "XXX" ["XXX" "XXX"]}})

(defn get-starting-points
  {:test (fn []
           (is= (get-starting-points test-map-3)
                ["11A" "22A"]))}
  [the-map]
  (->> (:map the-map)
       (keys)
       (filter (fn [k] (ends-with? k "A")))
       (sort)))

(comment
  (loop [state (->> (get-starting-points the-map)
                    (map (fn [p] [0 p])))
         steps 0]
    (when (zero? (rem steps 10000)) (println steps))
    (let [result (map (fn [s] (apply walk-a-step the-map s)) state)
          steps (inc steps)]
      (if (->> result
               (map second)
               (reduce (fn [a v] (and a (ends-with? v "Z")))
                       true))
        steps
        (recur result steps))))
  )

(defn endpoint? [p] (ends-with? p "Z"))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(def steps-from-start-to-end
  (->> (get-starting-points the-map)
       (map (fn [p] (walk-to-end the-map p endpoint?)))))

(def steps-from-end-to-end
  (->> steps-from-start-to-end
       (map second)
       (map (fn [p] (walk-to-end the-map p endpoint?)))))

(def cycles [18157 19241 12737 16531 14363 19783])

(reduce (fn [a n]
          (assoc a n (->> cycles
                          (remove (fn [x] (= x n)))
                          (map (fn [x] (gcd x n))))))
        {}
        cycles)
; {18157 (271 271 271 271 271),
;  19241 (271 271 271 271 271),
;  12737 (271 271 271 271 271),
;  16531 (271 271 271 271 271),
;  14363 (271 271 271 271 271),
;  19783 (271 271 271 271 271)}

(deftest puzzle-b
  (is= (->> cycles
            (map (fn [n] (/ n 271)))
            (reduce *)
            (* 271))
       14299763833181))




























