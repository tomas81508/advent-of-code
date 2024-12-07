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

(defn out-of-bound?
  [{size :size} [x y]]
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
    (let [direction (:direction state)
          next-position (map + current direction)]
      (cond (contains? (:obstacles atlas) next-position)
            (-> state
                (assoc :position current)
                (assoc :direction (turn-right direction))
                (update :visited (fn [visited]
                                   (reduce (fn [visited position]
                                             (if-not (contains? visited position)
                                               (assoc visited position #{direction})
                                               (update visited position conj direction)))
                                           visited
                                           walk))))

            (out-of-bound? atlas next-position)
            (-> state
                (assoc :position :out-of-bound)
                (update :visited (fn [visited]
                                   (reduce (fn [visited position]
                                             (if-not (contains? visited position)
                                               (assoc visited position #{direction})
                                               (update visited position conj direction)))
                                           visited
                                           walk))))

            (deja-vu? state next-position direction)
            :loop

            :else
            (recur (conj walk next-position)
                   next-position))))
  )

(defn move-until-the-end
  [atlas state]
  (loop [state state]
    (let [state (move-until-obstacle-and-turn atlas state)]
      (cond (= (:position state) :out-of-bound)
            state

            (= state :loop)
            :loop

            :else
            (recur state)))))

(comment
  (is= (time (-> (move-until-the-end atlas state)
                 (:visited)
                 (keys)
                 (count)))
       5153)
  )

(defn count-number-of-loops
  {:test (fn []
           (is= (count-number-of-loops test-atlas test-state) 6))}
  [atlas state]
  (->> (move-until-the-end atlas state)
       (:visited)
       (keys)
       (pmap (fn [p] (let [atlas (update atlas :obstacles conj p)]
                      (move-until-the-end atlas state))))
       (filter (fn [x] (= x :loop)))
       (count)))

(comment
  (is= (time (count-number-of-loops atlas state))
       1711)
  )





