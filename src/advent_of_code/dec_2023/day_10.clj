(ns advent-of-code.dec-2023.day-10
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_10_input.txt")
                (split-lines)))

(def start-position (loop [y 0]
                      (let [row (get input y)]
                        (if-let [p (->> row
                                        (map-indexed (fn [x c] [x c]))
                                        (some (fn [[x c]] (when (= c \S) [x y]))))]
                          p
                          (recur (inc y))))))

(def test-input-loop ["F-7"
                      "|.|"
                      "L-J"])

(def test-input ["7-F7-"
                 ".FJ|7"
                 "SJLL7"
                 "|F--J"
                 "LJ.LJ"])

(def state {:current   [0 2]
            :direction [1 0]})

(defn new-direction
  [direction character]
  ({[[1 0] \-]  [1 0]
    [[-1 0] \-] [-1 0]
    [[0 1] \|]  [0 1]
    [[0 -1] \|] [0 -1]
    [[1 0] \7]  [0 1]
    [[0 -1] \7] [-1 0]
    [[0 -1] \F] [1 0]
    [[-1 0] \F] [0 1]
    [[-1 0] \L] [0 -1]
    [[0 1] \L]  [1 0]
    [[1 0] \J]  [0 -1]
    [[0 1] \J]  [-1 0]}
   [direction character]))

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-input {:current [0 2] :direction [1 0]})
                {:current [1 2] :direction [0 -1]}))}
  [input state]
  (let [current (map + (:current state) (:direction state))
        new-symbol (get-in input (reverse current))
        new-direction (new-direction (:direction state) new-symbol)]
    (if (and (not= new-symbol \.) new-direction)
      {:current   current
       :direction new-direction}
      (when (= new-symbol \S)
        {:end current}))))

(defn walk-until-the-end
  {:test (fn []
           (is= (walk-until-the-end test-input {:current [0 2] :direction [1 0]})
                16)
           (is= (walk-until-the-end test-input {:current [0 2] :direction [0 1]})
                16)
           (is-not (walk-until-the-end test-input {:current [0 2] :direction [-1 0]}))
           (is-not (walk-until-the-end test-input {:current [0 2] :direction [0 -1]})))}
  [input state]
  (loop [state state
         steps 0]
    (cond (nil? state) nil
          (:end state) steps
          :else (recur (walk-a-step input state) (inc steps)))))

(deftest puzzle-a
  (is= (time (/ (walk-until-the-end input {:current start-position :direction [0 1]}) 2))
       ; "Elapsed time: 15.162173 msecs"
       6856)
  )

