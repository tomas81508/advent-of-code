(ns advent-of-code.dec-2018.day-23
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :refer [split-lines]]
            [clojure.edn :as edn]))

(def input (->> (slurp "src/advent_of_code/dec_2018/day_23_input.txt")
                (split-lines)))

(def test-input ["pos=<0,0,0>, r=4"
                 "pos=<1,0,0>, r=1"
                 "pos=<4,0,0>, r=3"
                 "pos=<0,2,0>, r=1"
                 "pos=<0,5,0>, r=3"
                 "pos=<0,0,3>, r=1"
                 "pos=<1,1,1>, r=1"
                 "pos=<1,1,2>, r=1"
                 "pos=<1,3,1>, r=1"])

(defn parse-nanobots
  {:test (fn []
           (is= (parse-nanobots test-input)
                [{:position [0 0 0], :radius 4}
                 {:position [1 0 0], :radius 1}
                 {:position [4 0 0], :radius 3}
                 {:position [0 2 0], :radius 1}
                 {:position [0 5 0], :radius 3}
                 {:position [0 0 3], :radius 1}
                 {:position [1 1 1], :radius 1}
                 {:position [1 1 2], :radius 1}
                 {:position [1 3 1], :radius 1}]))}
  [input]
  (->> input
       (map (fn [row]
              (let [[x y z r] (->> (re-seq #"-?\d+" row)
                                   (map edn/read-string))]
                {:position [x y z] :radius r})))))

(def test-nanobots (parse-nanobots test-input))
(def nanobots (parse-nanobots input))

(defn get-strongest-nanobot
  {:test (fn []
           (is= (get-strongest-nanobot test-nanobots)
                {:position [0 0 0], :radius 4}))}
  [nanobots]
  (->> nanobots
       (reduce (fn [a n] (if (> (:radius n) (:radius a)) n a))
               (first nanobots))))

(def strongest-test-nanobot (get-strongest-nanobot test-nanobots))
(def strongest-nanobot (get-strongest-nanobot nanobots))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [3 1 6] [2 3 -1]) 10))}
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (reduce +)))

(defn in-range?
  {:test (fn []
           (is (in-range? [0 0 0] [1 0 0] 4))
           (is-not (in-range? [0 0 0] [0 2 0] 1)))}
  [position other-position signal-radius]
  (<= (manhattan-distance position other-position) signal-radius))

(defn nanobots-in-range
  {:test (fn []
           (is= (count (nanobots-in-range test-nanobots strongest-test-nanobot))
                7))}
  [nanobots nanobot]
  (->> nanobots
       (filter (fn [n] (in-range? (:position nanobot) (:position n) (:radius nanobot))))))

(comment
  (time (count (nanobots-in-range nanobots strongest-nanobot)))
  ; 580
  )

; part 2

(def test-input-2 ["pos=<10,12,12>, r=2"
                   "pos=<12,14,12>, r=2"
                   "pos=<16,12,12>, r=4"
                   "pos=<14,14,14>, r=6"
                   "pos=<50,50,50>, r=200"
                   "pos=<10,10,10>, r=5"])

(def test-nanobots-2 (parse-nanobots test-input-2))

(defn third [[_ _ z]] z)

(defn calculate-nanobots-in-range
  [nanobots position]
  (->> nanobots
       (filter (fn [n] (in-range? (:position n) position (:radius n))))
       (count)))

(defn find-better-position
  [nanobots position]
  (loop [distance 6
         factor 10000000
         start [position
                (calculate-nanobots-in-range nanobots position)
                (manhattan-distance position [0 0 0])]]
    (let [next (loop [[position nanobots-in-range distance-to-origin :as current] start]
                 (println "[" factor "] looping with" position distance-to-origin nanobots-in-range)
                 (let [best (->> (for [x (range (- (first position) (* factor distance)) (+ (first position) (inc (* factor distance))) factor)
                                       y (range (- (second position) (* factor distance)) (+ (second position) (inc (* factor distance))) factor)
                                       z (range (- (third position) (* factor distance)) (+ (third position) (inc (* factor distance))) factor)]
                                   [x y z])
                                 (reduce (fn [a p] (conj a [p (calculate-nanobots-in-range nanobots p)]))
                                         [])
                                 (filter (fn [[_ n]] (>= n nanobots-in-range)))
                                 (map (fn [[p n]] [p n (manhattan-distance p [0 0 0])]))
                                 (sort-by (juxt (comp - second) first))
                                 (first))]
                   (if (= best current)
                     best
                     (recur best))))]
      (if (= factor 1)
        next
        (recur distance (quot factor 10) next)))))

(comment
  (time (find-better-position nanobots [0 0 0]))
  ; "Elapsed time: 12901.778208 msecs"
  ; => [[13839482 57977321 25999544] 978 97816347]
  )






