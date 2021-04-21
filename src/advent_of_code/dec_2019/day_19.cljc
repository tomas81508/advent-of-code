(ns advent-of-code.dec-2019.day-19
  (:require [ysera.test :refer [is is= is-not]]
            [advent-of-code.dec-2019.day-09 :refer [run
                                                    create-program]]))

(def puzzle-input (as-> (slurp "src/advent_of_code/dec_2019/day_19.txt") $
                        (clojure.string/split $ #",")
                        (map read-string $)
                        (into [] $)))

(def program (create-program puzzle-input [0 0]))

(run program)

(defn beam-point?
  {:test (fn []
           (is (beam-point? [0 0]))
           (is-not (beam-point? [1 0])))}
  [point]
  (= 1 (first (:outputs (run puzzle-input point)))))

(defn show-beam
  ([size] (show-beam size [0 0]))
  ([size [x y]]
   (->> (for [y (range y (+ y size))
              x (range x (+ x size))]
          [x y])
        (map beam-point?)
        (partition size)
        (map (fn [row]
               (->> row
                    (map (fn [p] (if p "#" ".")))
                    (clojure.string/join)))))))

(show-beam 50)
(show-beam 20 [39 39])

(defn calculate-beam-points
  [size]
  (->> (for [y (range size)
             x (range size)]
         [x y])
       (filter beam-point?)
       (count)))

(calculate-beam-points 50)
; 209

(defn beams-for-row
  [y]
  (if (beam-point? [y y])
    "ERROR"
    (->> (loop [y y
                x y
                result []
                beam-present-for-y false]
           (let [beam (beam-point? [x y])]
             (cond (and (not beam) beam-present-for-y)
                   result

                   (not beam)
                   (recur y (inc x) result beam-present-for-y)

                   :else
                   (recur y (inc x) (conj result [x y]) true))))
         (count))))

(beams-for-row 900)
; => 201

(defn fit-ship?
  [[x y] size]
  (->> (for [y (range y (+ y size))
             x (range x (+ x size))]
         [x y])
       (every? beam-point?)))

(fit-ship? [45 39] 5)

(fit-ship? [1100 1000] 10)

(show-beam 100 [1039 900])
(show-beam 100 [1040 901])
(show-beam 100 [1041 901])
(show-beam 100 [1041 902])
(show-beam 100 [1042 902])
(show-beam 100 [1042 903])
(show-beam 100 [1043 903])
(show-beam 100 [1043 904])
(show-beam 100 [1044 904])
(show-beam 100 [1044 905])
(show-beam 100 [1045 905]) ; only beam here

(+ (* 1045 10000) 905)





