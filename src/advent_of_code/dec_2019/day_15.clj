(ns advent-of-code.dec-2019.day-15
  (:require [ysera.test :refer [is is-not is= deftest]]
            [clojure.string :refer [join]]
            [clojure.set :refer [map-invert difference union]]
            [advent-of-code.dec-2019.day-09 :refer [run
                                                    run-instruction
                                                    create-program]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_15.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)))

(def direction->input
  {[0 -1] 1 [0 1] 2 [-1 0] 3 [1 0] 4})

(defn search-the-area-for-distance
  [state-atom position distance program]
  (let [directions [[-1 0] [0 -1] [1 0] [0 1]]]
    (doseq [d directions]
      (let [new-position (into [] (map + position d))]
        (when-not (contains? (:cells (deref state-atom)) new-position)
          (let [input (direction->input d)
                program (run program [input])
                output (first (:outputs program))
                program (assoc program :outputs [])]
            (swap! state-atom
                   (fn [state]
                     (condp = output
                       0 (assoc-in state [:cells new-position] {:type     :wall
                                                                :distance distance})
                       1 (assoc-in state [:cells new-position] {:type     :space
                                                                :distance distance})
                       2 (assoc-in state [:cells new-position] {:type     :oxygen-system
                                                                :distance distance}))))
            (when (or (= output 1) (= output 2))
              (search-the-area-for-distance state-atom
                                            new-position
                                            (inc distance)
                                            program))))))))

(defn search-the-area
  [int-code]
  (let [program (create-program int-code [])
        position [0 0]
        state-atom (atom {:cells {[0 0] {:type     :start
                                         :distance 0}}})]
    (search-the-area-for-distance state-atom position 1 program)
    (deref state-atom)))

(deftest puzzle-a
         (is= (time (->> (get-puzzle-input)
                         (search-the-area)
                         (:cells)
                         (vals)
                         (filter (fn [{type :type}] (= type :oxygen-system)))
                         (first)
                         (:distance)))
              ; "Elapsed time: 188.711565 msecs"
              230))

(def atlas (->> (get-puzzle-input)
                (search-the-area)))

(defn calculate-max-distance
  [atlas position]
  (loop [distance 0
         known-cells #{position}
         boundary-cells #{position}]
    (let [directions [[-1 0] [1 0] [0 -1] [0 1]]
          new-cells (->> boundary-cells
                         (reduce (fn [a bc]
                                   (let [new-cells (as-> directions $
                                                         (map (fn [d] (vec (map + d bc))) $)
                                                         (filter (fn [p] (not= (get-in atlas [:cells p :type]) :wall)) $)
                                                         (set $)
                                                         (difference $ known-cells))]
                                     (union a new-cells)))
                                 #{}))]
      (if (empty? new-cells)
        distance
        (recur (inc distance)
               (union known-cells new-cells)
               new-cells)))))

(deftest puzzle-b
         (is= (time (calculate-max-distance atlas [12 14]))
              288))

