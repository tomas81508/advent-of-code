(ns advent-of-code.dec-2022.day-24
  (:require [advent-of-code.test :refer [is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_24_input.txt")
                (clojure.string/split-lines)))

(def test-input ["#.######"
                 "#>>.<^<#"
                 "#.<..<<#"
                 "#>v.><>#"
                 "#<^v^^>#"
                 "######.#"])

(def test-state {:right     #{[1 1] [2 1] [1 3] [4 3] [6 3] [6 4]}
                 :left      #{[4 1] [6 1] [2 2] [5 2] [6 2] [5 3] [1 4]}
                 :up        #{[5 1] [2 4] [4 4] [5 4]}
                 :down      #{[2 3] [3 4]}
                 :size      [7 5]
                 :positions #{[1 0]}})

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                test-state))}
  [input]
  (let [width (count (first input))
        height (count input)]
    (->> (for [y (range height)
               x (range width)]
           [x y])
         (reduce (fn [a [x y :as point]]
                   (let [c (get-in input [y x])]
                     (condp = c
                       \# a
                       \. a
                       \> (update a :right conj point)
                       \< (update a :left conj point)
                       \^ (update a :up conj point)
                       \v (update a :down conj point))))
                 {:right     #{}
                  :left      #{}
                  :up        #{}
                  :down      #{}
                  :size      [(dec width) (dec height)]
                  :positions #{[1 0]}}))))

(defn move-right
  {:test (fn []
           (is= (move-right #{[1 1] [2 1] [1 3] [4 3] [6 3] [6 4]} 7)
                #{[2 1] [3 1] [2 3] [5 3] [1 3] [1 4]}))}
  [blizzards size]
  (->> blizzards
       (map (fn [[x y]] (if (= (inc x) size) [1 y] [(inc x) y])))
       (set)))

(defn move-left
  {:test (fn []
           (is= (move-left #{[4 1] [6 1] [2 2] [5 2] [6 2] [5 3] [1 4]} 7)
                #{[3 1] [5 1] [1 2] [4 2] [5 2] [4 3] [6 4]}))}
  [blizzards size]
  (->> blizzards
       (map (fn [[x y]] (if (= (dec x) 0) [(dec size) y] [(dec x) y])))
       (set)))

(defn move-up
  {:test (fn []
           (is= (move-up #{[5 1] [2 4] [4 4] [5 4]} 5)
                #{[5 4] [2 3] [4 3] [5 3]}))}
  [blizzards size]
  (->> blizzards
       (map (fn [[x y]] (if (= (dec y) 0) [x (dec size)] [x (dec y)])))
       (set)))

(defn move-down
  {:test (fn []
           (is= (move-down #{[2 3] [3 4]} 5)
                #{[2 4] [3 1]}))}
  [blizzards size]
  (->> blizzards
       (map (fn [[x y]] (if (= (inc y) size) [x 1] [x (inc y)])))
       (set)))

(defn get-maybe-new-positions
  {:test (fn []
           (is= (get-maybe-new-positions {:positions #{[1 1]} :size [7 5]})
                #{[1 1] [2 1] [1 2] [1 0]})
           (is= (get-maybe-new-positions {:positions #{[1 1] [6 4]} :size [7 5]})
                #{[1 1] [2 1] [1 2] [1 0] [5 4] [6 4] [6 3] [6 5]}))}
  [{positions :positions [w h] :size}]
  (let [directions [[-1 0] [1 0] [0 -1] [0 1]]]
    (reduce (fn [a coordinate]
              (let [additions (->> directions
                                   (map (fn [d] (map + coordinate d)))
                                   (filter (fn [[x y :as c]] (or (and (< 0 x w) (< 0 y h))
                                                                 (= c [(dec w) h])
                                                                 (= c [1 0]))))
                                   (set))]
                (clojure.set/union a additions)))
            positions
            positions)))

(defn move-a-step
  {:test (fn []
           (is= (-> (create-state test-input)
                    (move-a-step)
                    (:positions))
                #{[1 1] [1 0]}))}
  [state]
  (as-> state $
        (update $ :right move-right (first (:size state)))
        (update $ :left move-left (first (:size state)))
        (update $ :up move-up (second (:size state)))
        (update $ :down move-down (second (:size state)))
        (update $ :positions
                (fn [_]
                  (let [maybe-new-positions (get-maybe-new-positions $)
                        blizzard (clojure.set/union (:right $) (:left $) (:up $) (:down $))]
                    (clojure.set/difference maybe-new-positions blizzard))))))

(defn move-until-goal
  {:test (fn []
           (is= (-> (create-state test-input)
                    (move-until-goal))
                18))}
  [state]
  (let [goal (map + (:size state) [-1 0])]
    (loop [state state
           steps 0]
      (if (contains? (:positions state) goal)
        steps
        (recur (move-a-step state) (inc steps))))))

(defn move-to-goal-back-and-to-goal
  {:test (fn []
           (is= (-> (create-state test-input)
                    (move-to-goal-back-and-to-goal))
                54))}
  [state]
  (let [goal (map + (:size state) [-1 0])
        goals [goal [1 0] goal]]
    (loop [state state
           steps 0
           goals goals]
      (cond (empty? goals)
            (dec steps)

            (contains? (:positions state) (first goals))
            (recur (move-a-step (assoc state :positions #{(first goals)}))
                   (inc steps)
                   (rest goals))
            :else
            (recur (move-a-step state)
                   (inc steps)
                   goals)))))


(comment
  (time
    (-> (create-state input)
        (move-until-goal)))
  ; "Elapsed time: 1604.259217 msecs"
  ; => 322

  (time
    (-> (create-state input)
        (move-to-goal-back-and-to-goal)))
  ; "Elapsed time: 4149.099066 msecs"
  ; => 974
  )