(ns advent-of-code.dec-2023.day-24b
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.test :refer [deftest]]))

(defn parse-input
  {:test (fn []
           (is= (parse-input "19, 13, 30 @ -2,  1, -2")
                {:point [19 13 30] :velocity [-2 1 -2]}))}
  [line]
  (let [[px py pz vx vy vz] (->> (re-seq #"[\d-]+" line)
                                 (map read-string))]
    {:point [px py pz] :velocity [vx vy vz]}))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_24_input.txt")
                (clojure.string/split-lines)
                (map parse-input)
                (into [])))

(def test-input (->> ["19, 13, 30 @ -2,  1, -2"
                      "18, 19, 22 @ -1, -1, -2"
                      "20, 25, 34 @ -2, -2, -4"
                      "12, 31, 28 @ -1, -2, -1"
                      "20, 19, 15 @  1, -5, -3"]
                     (map parse-input)
                     (into [])))

(defn find-values
  [p1 p2 hv]
  (reduce (fn [a v]
            (if (and (not (zero? (- v hv)))
                     (zero? (rem (- p1 p2) (- v hv))))
              (conj a v)
              a))
          []
          (concat (range -1000 0)
                  (range 1 1001))))

(defn find-velocity
  {:test (fn []
           (is= (find-velocity test-input)
                [#{-3} #{-4 1 4 -1 -8 -3 -5} #{-4 -1 -6 -3 6 2 -10}]))}
  [data]
  (reduce (fn [a i]
            (let [same-velocity-points (->> data
                                            (map :velocity)
                                            (map (fn [v] (nth v i)))
                                            (frequencies)
                                            (filter (fn [[_ v]] (>= v 2)))
                                            (map first)
                                            (map (fn [vv] (->> data
                                                               (filter (fn [d] (= vv (nth (:velocity d) i))))))))
                  values (->> same-velocity-points
                              (map (fn [[p1 p2]]
                                     (set (find-values (nth (:point p1) i)
                                                       (nth (:point p2) i)
                                                       (nth (:velocity p1) i)))))
                              (apply clojure.set/intersection))]
              (conj a values)))
          []
          (range 3)))

(comment
  (def velocity (->> (find-velocity input)
                     (map first)))
  ; My velocity [154 75 290]
  )

(defn find-points-with-same-velocity-parts
  [data velocity]
  (->> velocity
       (map-indexed
         (fn [index vp]
           (->> data
                (some (fn [{v :velocity :as p}] (when (= (nth v index) vp) p))))))))

(comment
  (find-points-with-same-velocity-parts input [154 75 290])
  ; => (nil nil {:point [227838059648283 388499788178823 127971479302113], :velocity [52 -241 290]})
  )

(def pz 127971479302113)

; Need to find px and py

; My two first points
; 315268300752660, 284016300325583, 407533418983227 @ -11, 23, -52

; an my velocity [154 75 290]

; px - 315268300752660 = t_1 (154 - (-11)) = 165 t_1
; py - 284016300325583 = t_1 (75 - 23) = 52 t_1
; 127971479302113 - 407533418983227 = t_1 (290 - (-52)) = 342 t_1

; =>
(comment
  (- 127971479302113 407533418983227)
  ; =>
  -279561939681114

  (def t_1 (/ -279561939681114 342))
  (def py (+ 284016300325583 (* 52 t_1)))
  (def px (+ 315268300752660 (* 165 t_1)))

  (def solution (+ px py pz))

  )


