(ns advent-of-code.dec-2024.day-06b
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is is-not]]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_06_input.txt")
                (clojure.string/split-lines)))

(def test-input ["....#....."
                 ".........#"
                 ".........."
                 "..#......."
                 ".......#.."
                 ".........."
                 ".#..^....."
                 "........#."
                 "#........."
                 "......#..."])

(defn create-atlas
  {:test (fn []
           (is= (create-atlas test-input)
                {:obstacles #{[8 7] [2 3] [7 4] [9 1] [6 9] [1 6] [0 8] [4 0]}
                 :size      10}))}
  [input]
  (->> input
       (reduce-kv (fn [a y row]
                    (->> (into [] row)
                         (reduce-kv (fn [a x c]
                                      (if-not (= c \#)
                                        a
                                        (update a :obstacles conj [x y])))
                                    a)))
                  {:obstacles #{}
                   :size      (count input)})))

(def test-atlas (create-atlas test-input))
(def atlas (create-atlas input))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:position  [4 6]
                 :direction [0 -1]
                 :visited   {[4 6] #{[0 -1]}}}))}
  [input]
  (let [position (->> input
                      (reduce-kv (fn [_ y row]
                                   (let [result (->> (into [] row)
                                                     (reduce-kv (fn [_ x c] (when (= c \^) (reduced [x y])))
                                                                nil))]
                                     (when result (reduced result))))
                                 nil))
        direction [0 -1]]
    {:position  position
     :direction direction
     :visited   {position #{direction}}}))

(def test-state (create-state test-input))
(def state (create-state input))

(def turn-right
  {[1 0]  [0 1]
   [0 1]  [-1 0]
   [-1 0] [0 -1]
   [0 -1] [1 0]})

(defn out-of-bound? [{size :size} [x y]]
  (or (< x 0)
      (> x (dec size))
      (< y 0)
      (> y (dec size))))

(defn deja-vu?
  [state position direction]
  (when-let [directions (get-in state [:visited position])]
    (contains? directions direction)))

(defn move-until-obstacle-and-turn
  {:test (fn []
           (is= (move-until-obstacle-and-turn test-atlas test-state)
                {:position  [4 1]
                 :direction [1 0]
                 :visited   {[4 6] #{[0 -1]} [4 5] #{[0 -1]} [4 4] #{[0 -1]} [4 3] #{[0 -1]} [4 2] #{[0 -1]} [4 1] #{[0 -1]}}}))}
  [atlas state]
  (loop [walk #{}
         current (:position state)]
    (let [next-position (map + current (:direction state))]
      ;(println next-position)
      (cond (contains? (:obstacles atlas) next-position)
            (do                                             ;(println "Hit an obstacle")
              (-> state
                  (assoc :position current)
                  (assoc :direction (turn-right (:direction state)))
                  (update :visited (fn [visited]
                                     (reduce (fn [visited position]
                                               (if-not (contains? visited position)
                                                 (assoc visited position #{(:direction state)})
                                                 (update visited conj #{(:direction state)})))
                                             visited
                                             walk)))))

            (out-of-bound? atlas next-position)
            :escaped

            (deja-vu? state next-position (:direction state))
            :loop

            :else
            (recur (conj walk next-position)
                   next-position))))
  )

(defn move-until-the-end-and-count-visited
  [atlas state]
  (loop [state state]
    (let [state (move-until-obstacle-and-turn atlas state)]
      (if (keyword? state)
        state
        (recur state)))))

(comment
  (move-until-the-end-and-count-visited test-atlas test-state)
  (time (move-until-the-end-and-count-visited atlas state))
  )

(->> (for [x (range (:size test-atlas))
      y (range (:size test-atlas))]
  [x y])
     (map (fn [p] (let [modified-atlas (update test-atlas :obstacles conj p)]
                    ; TODO: not when guard at p or already obstacle
                    
                    ))))



; part A





