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

(defn find-bounding-rectangle
  {:test (fn []
           (is= (find-bounding-rectangle test-nanobots-2)
                [[10 10 10] [50 50 50]])
           (is= (find-bounding-rectangle test-nanobots)
                [[0 0 0] [4 5 3]]))}
  [nanobots]
  (reduce (fn [a {position :position}]
            (-> a
                (update 0 (fn [min-values] (mapv min min-values position)))
                (update 1 (fn [max-values] (mapv max max-values position)))))
          [(:position (first nanobots)) (:position (first nanobots))]
          nanobots))

(defn get-range-step
  {:test (fn []
           (is= (get-range-step 10 50 10) 4)
           (is= (get-range-step 10 14 10) 1))}
  [start stop max-steps]
  (let [distance (- stop start)
        step (quot distance max-steps)]
    (if (zero? step) 1 step)))

(defn third [[_ _ z]] z)

(defn find-better-rectangle
  {:test (fn []
           (is= (find-better-rectangle test-nanobots-2 [[10 10 10] [50 50 50]] 10)
                [[6 6 6] [14 14 14]])
           (is= (find-better-rectangle test-nanobots-2 [[8 8 8] [12 12 12]] 10)
                {:done [12 12 12]}))}
  [nanobots rectangle max-steps]
  (let [x-range (mapv first rectangle)
        y-range (mapv second rectangle)
        z-range (mapv third rectangle)
        x-range-step (apply get-range-step (conj x-range max-steps))
        y-range-step (apply get-range-step (conj y-range max-steps))
        z-range-step (apply get-range-step (conj z-range max-steps))
        best-position (->> (for [x (range (first x-range) (inc (second x-range)) x-range-step)
                                 y (range (first y-range) (inc (second y-range)) y-range-step)
                                 z (range (first z-range) (inc (second z-range)) z-range-step)]
                             [x y z])
                           (reduce (fn [a position]
                                     (let [number-in-range (->> nanobots
                                                                (filter (fn [n] (in-range? (:position n) position (:radius n))))
                                                                (count))]
                                       (cond (< number-in-range (:number-in-range a)) a
                                             (> number-in-range (:number-in-range a)) {:number-in-range number-in-range :position position}
                                             :else (if (< (manhattan-distance [0 0 0] position)
                                                          (manhattan-distance [0 0 0] (:position a)))
                                                     {:number-in-range number-in-range :position position}
                                                     a))))
                                   {:number-in-range 0
                                    :position        nil})
                           (:position))]
    (if (= x-range-step y-range-step z-range-step 1)
      {:done best-position}
      [[(- (first best-position) x-range-step)
        (- (second best-position) y-range-step)
        (- (third best-position) z-range-step)]
       [(+ (first best-position) x-range-step)
        (+ (second best-position) y-range-step)
        (+ (third best-position) z-range-step)]])
    ))

(defn find-coordinates-in-range-of-the-largets-number-of-nanobots
  {:test (fn []
           (is= (find-coordinates-in-range-of-the-largets-number-of-nanobots test-nanobots-2)
                36))}
  ([nanobots]
   (find-coordinates-in-range-of-the-largets-number-of-nanobots
     nanobots
     (find-bounding-rectangle nanobots) 30))
  ([nanobots initial-rectangle max-steps]
   (loop [rectangle initial-rectangle]
     (println rectangle)
     (if (:done rectangle)
       (do (println (:done rectangle))
           (manhattan-distance [0 0 0] (:done rectangle)))
       (recur (find-better-rectangle nanobots rectangle max-steps))))))

(comment
  (time (find-coordinates-in-range-of-the-largets-number-of-nanobots nanobots))
  ; 100592453 [14315582 59365374 26911497]
  ; 20 -> 100607191 [14306083 59372743 26928365]
  ; 100038641 [14950629 58303297 26784715]

  (def initial-rectangle (find-bounding-rectangle nanobots))
  (find-better-rectangle nanobots initial-rectangle 20)
  (find-better-rectangle nanobots
                         [[0 0 0] [19492980 62842408 29903069]]
                         20)
  (find-coordinates-in-range-of-the-largets-number-of-nanobots
    nanobots
    (find-bounding-rectangle nanobots)
    25)

  )






