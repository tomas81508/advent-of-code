(ns advent-of-code.dec-2017.day-03
  (:require [ysera.test :refer [deftest is=]]
            [clojure.string :as string]))


(defn get-level
  {:test (fn []
           (is= (get-level 1) 0)
           (is= (get-level 2) 1)
           (is= (get-level 8) 1)
           (is= (get-level 9) 1)
           (is= (get-level 10) 2)
           (is= (get-level 25) 2)
           (is= (get-level 26) 3)
           (is= (get-level 49) 3)
           (is= (get-level 50) 4)
           (is= (get-level 277678) 263))}
  [n]
  (let [x (int (Math/ceil (Math/sqrt n)))]
    (/ (if (odd? x)
         (dec x)
         x)
       2)))

(defn get-end-number
  {:test (fn []
           (is= (get-end-number 1) 9)
           (is= (get-end-number 2) 25)
           (is= (get-end-number 3) 49))}
  [level]
  (int (Math/pow (inc (* 2 level)) 2)))

(defn get-start-number
  {:test (fn []
           (is= (get-start-number 1) 2)
           (is= (get-start-number 2) 10)
           (is= (get-start-number 3) 26))}
  [level]
  (inc (get-end-number (dec level))))

(defn get-width
  {:test (fn []
           (is= (get-width 1) 2)
           (is= (get-width 2) 4)
           (is= (get-width 3) 6))}
  [level]
  (* 2 level))


(defn steps-to-center
  {:test (fn []
           (is= (steps-to-center 0 3) 1)
           (is= (steps-to-center 1 3) 0)
           (is= (steps-to-center 2 3) 1)
           (is= (steps-to-center 3 3) 2)
           (is= (steps-to-center 0 5) 2))}
  [n range]
  (let [middle (int (Math/floor (/ range 2)))]
    (Math/abs (- n middle))))


(defn steps
  {:test (fn []
           (is= (steps 1) 0)
           (is= (steps 12) 3)
           (is= (steps 23) 2)
           (is= (steps 1024) 31))}
  [n]
  (if (= n 1)
    0
    (let [level (get-level n)
          start-number (get-start-number level)
          width (get-width level)
          y-number (mod (- n start-number) width)]

      (+ level (steps-to-center y-number (dec width))))))

(deftest answer-3a (is= (steps 277678) 475))

;; Alternative

(defn abs [x] (if (pos? x) x (- x)))

(def north [0 1])
(def west [-1 0])
(def south [0 -1])
(def east [1 0])


(defn get-direction
  {:test (fn []
           (is= (get-direction [0 0]) east)
           (is= (get-direction [1 0]) north)
           (is= (get-direction [1 1]) west)
           (is= (get-direction [0 1]) west)
           (is= (get-direction [-1 1]) south)
           (is= (get-direction [-1 0]) south)
           (is= (get-direction [-1 -1]) east)
           (is= (get-direction [1 -1]) east)
           (is= (get-direction [2 -1]) north)
           (is= (get-direction [2 0]) north)
           (is= (get-direction [2 2]) west)
           (is= (get-direction [0 2]) west)
           (is= (get-direction [-2 2]) south)
           (is= (get-direction [-2 0]) south)
           (is= (get-direction [-2 -2]) east)
           (is= (get-direction [0 -2]) east))}
  [[x y]]
  (cond (= x y 0) east
        (and (= x y) (neg? x)) east
        (= x y) west
        (and (= x (- y)) (pos? x)) east
        (= x (- y)) south
        (and (pos? y) (< (abs x) y)) west
        (< (abs x) (- y)) east
        (and (pos? x) (< (abs y) x)) north
        (< (abs y) (- x)) south))

(defn get-coordinate
  {:test (fn []
           (is= (get-coordinate 2)
                [1 0]))}
  [n]
  (reduce (fn [coordinate _]
            (doall (map + coordinate (get-direction coordinate))))
          [0 0]
          (range 1 n)))

(defn taxi-distance
  {:test (fn []
           (is= (taxi-distance [1 3] [4 4]) 4))}
  [c1 c2]
  (->> (map - c1 c2)
       (map abs)
       (apply +)))

(deftest answer-3a-alt
         (is= (taxi-distance (get-coordinate 1) [0 0])
              0)
         (is= (taxi-distance (get-coordinate 12) [0 0])
              3)
         (is= (taxi-distance (get-coordinate 23) [0 0])
              2)
         (is= (taxi-distance (get-coordinate 1024) [0 0])
              31)
         (is= (taxi-distance (get-coordinate 277678) [0 0])
              475))


