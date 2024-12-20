(ns advent-of-code.dec-2022.day-23
  (:require [advent-of-code.test :refer [is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_23_input.txt")
                (clojure.string/split-lines)))

(def test-input ["....."
                 "..##."
                 "..#.."
                 "....."
                 "..##."
                 "....."])

(def large-test-input [".............."
                       ".............."
                       ".......#......"
                       ".....###.#...."
                       "...#...#.#...."
                       "....#...##...."
                       "...#.###......"
                       "...##.#.##...."
                       "....#..#......"
                       ".............."
                       ".............."
                       ".............."])

(defn create-state [input]
  {:elves     (->> input
                   (map-indexed (fn [y xs]
                                  (->> xs
                                       (map-indexed (fn [x v]
                                                      (when (= v \#) [x y])))
                                       (remove nil?))))
                   (map set)
                   (reduce clojure.set/union))
   :direction :north})

(def state (create-state input))
(def test-state (create-state test-input))
(def large-test-state (create-state large-test-input))

(def direction->vectors {:north [[-1 -1] [0 -1] [1 -1]]
                         :east  [[1 -1] [1 0] [1 1]]
                         :south [[-1 1] [0 1] [1 1]]
                         :west  [[-1 -1] [-1 0] [-1 1]]})

(def direction->vector {:north [0 -1]
                        :east  [1 0]
                        :south [0 1]
                        :west  [-1 0]})

(defn get-adjacent-position
  {:test (fn []
           (is= (get-adjacent-position [1 1] :north)
                [[0 0] [1 0] [2 0]]))}
  [position direction]
  (->> (direction->vectors direction)
       (map (fn [d] (map + position d)))))

(def directions
  (for [x (range -1 2)
        y (range -1 2)
        :when (not (and (zero? x) (zero? y)))]
    [x y]))

(defn elves-at-positions?
  [positions elves]
  (not (empty? (clojure.set/intersection elves (set positions)))))

(defn move? [pos elves]
  (let [neighbours (->> directions
                        (map (fn [d] (map + d pos)))
                        (set))]
    (elves-at-positions? neighbours elves)))

(def next-direction {:north :south
                     :south :west
                     :west  :east
                     :east  :north})

(defn move
  {:test (fn []
           (is= (move test-state)
                {:elves     #{[2 0] [3 0] [2 2] [2 4] [3 3]}
                 :direction :south})
           (is= (-> test-state
                    (move)
                    (move)
                    (move)
                    (move))
                {:elves     #{[4 3] [2 5] [4 1] [0 2] [2 0]}
                 :direction :north}))}
  [state]
  {:elves     (->> (reduce (fn [a p]
                             (if-not (move? p (:elves state))
                               (assoc a p [p])
                               (loop [direction (:direction state)
                                      checked-directions 0]
                                 (if (= checked-directions 4)
                                   (update a p conj p)
                                   (let [nps (get-adjacent-position p direction)]
                                     (if-not (elves-at-positions? nps (:elves state))
                                       (update a (map + p (direction->vector direction)) conj p)
                                       (recur (next-direction direction) (inc checked-directions))))))))
                           {}
                           (:elves state))
                   (reduce-kv (fn [a k v]
                                (if (> (count v) 1)
                                  (reduce conj a v)
                                  (conj a k)))
                              #{}))
   :direction (next-direction (:direction state))})

(defn move-n [state n]
  (-> state
      ((apply comp (repeat n move)))))

(defn get-rectangle-area
  {:test (fn []
           (is= (let [s (move-n large-test-state 10)]
                  (- (get-rectangle-area s)
                     (count (:elves s))))
                110))}
  [state]
  (let [xs (map first (:elves state))
        min-x (apply min xs)
        max-x (apply max xs)
        ys (map second (:elves state))
        min-y (apply min ys)
        max-y (apply max ys)]
    (* (- max-x min-x -1) (- max-y min-y -1))))

(comment
  "Puzzle a"
  (time (let [s (move-n state 10)]
          (- (get-rectangle-area s)
             (count (:elves s)))))
  ; "Elapsed time: 897.791157 msecs"
  ; => 4025
  )

(defn move-until-the-end
  {:test (fn []
           (is= (move-until-the-end large-test-state)
                20))}
  [state]
  (loop [index 0
         state state]
    (let [next-state (move state)]
      (println index)
      (if (= (:elves state) (:elves next-state))
        (inc index)
        (recur (inc index) next-state)))))

(comment
  (time (move-until-the-end state))
  ; "Elapsed time: 45191.161734 msecs"
  ; => 935
  )