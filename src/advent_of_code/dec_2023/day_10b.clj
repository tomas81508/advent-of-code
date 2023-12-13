(ns advent-of-code.dec-2023.day-10b
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]))

; In 10b I stole the idea from Daniel Gullberg using an expanded state.

(def input (->> (slurp "src/advent_of_code/dec_2023/day_10_input.txt")
                (split-lines)))

(def test-input [".F-7"
                 "FJ.S"
                 "L--J"])

(defn create-state
  [input]
  (->> input
       (map-indexed (fn [y row] [y row]))
       (reduce (fn [a [y row]]
                 (->> row
                      (map-indexed (fn [x symbol] [x symbol]))
                      (reduce (fn [a [x symbol]]
                                (assoc a [x y] symbol))
                              a)))
               {})))

(def test-state (create-state test-input))

(defn get-start-position
  {:test (fn []
           (is= (get-start-position test-state)
                [3 1]))}
  [state]
  (->> state
       (some (fn [[k v]] (when (= v \S) k)))))

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
           (is= (walk-a-step test-state {:current [3 1] :direction [0 1]})
                {:current [3 2] :direction [-1 0]}))}
  [input-map state]
  (let [current (map + (:current state) (:direction state))
        new-symbol (input-map current)
        new-direction (new-direction (:direction state) new-symbol)]
    (if (and (not= new-symbol \.) new-direction)
      {:current   current
       :direction new-direction}
      (when (= new-symbol \S)
        {:end current}))))

(defn walk-until-the-end
  {:test (fn []
           (is= (walk-until-the-end test-state {:current [3 1] :direction [0 1]})
                10))}
  [state position]
  (loop [position position
         steps 0]
    (cond (nil? position) nil
          (:end position) steps
          :else (recur (walk-a-step state position) (inc steps)))))

(deftest puzzle-a
  (is= (time (let [state (create-state input)
                   start-position (get-start-position state)]
               (/ (walk-until-the-end state {:current start-position :direction [0 1]})
                  2)))
       ; "Elapsed time: 15.162173 msecs"
       6856)
  )

; part 2

(def test-input-b [".F----7F7F7F7F-7...."
                   ".|F--7||||||||FJ...."
                   ".||.FJ||||||||L7...."
                   "FJL7L7LJLJ||LJ.L-7.."
                   "L--J.L7...LJS7F-7L7."
                   "....F-J..F7FJ|L7L7L7"
                   "....L7.F7||L7|.L7L7|"
                   ".....|FJLJ|FJ|F7|.LJ"
                   "....FJL-7.||.||||..."
                   "....L---J.LJ.LJLJ..."])

(def test-input-c ["..........."
                   ".S-------7."
                   ".|F-----7|."
                   ".||.....||."
                   ".||.....||."
                   ".|L-7.F-J|."
                   ".|..|.|..|."
                   ".L--J.L--J."
                   "..........."])

(defn collect-loop-coordinates
  {:test (fn []
           (is= (collect-loop-coordinates test-state {:current [3 1] :direction [0 1]})
                #{[2 2] [1 0] [1 1] [3 0] [0 2] [2 0] [3 1] [1 2] [3 2] [0 1]}))}
  [state position]
  (loop [position position
         steps 0
         coordinates #{}]
    (cond (nil? position) nil
          (:end position) coordinates
          :else (recur (walk-a-step state position)
                       (inc steps)
                       (conj coordinates (:current position))))))


(defn create-expanded-data
  {:test (fn []
           (is= (create-expanded-data [2 1] \J)
                {[6 3] \. [7 3] \| [8 3] \.
                 [6 4] \- [7 4] \J [8 4] \.
                 [6 5] \. [7 5] \. [8 5] \.})
           (is= (create-expanded-data [0 0] \.)
                {[0 0] \. [1 0] \. [2 0] \.
                 [0 1] \. [1 1] \. [2 1] \.
                 [0 2] \. [1 2] \. [2 2] \.}))}
  [[x y] symbol]
  (let [x (* 3 x)
        y (* 3 y)]
    (if (= symbol \.)
      (zipmap (for [i (range x (+ x 3)) j (range y (+ y 3))] [i j]) (repeat \.))
      {[x y]             \.
       [(inc x) y]       (if (new-direction [0 1] symbol) \| \.)
       [(+ x 2) y]       \.

       [x (inc y)]       (if (new-direction [1 0] symbol) \- \.)
       [(inc x) (inc y)] symbol
       [(+ x 2) (inc y)] (if (new-direction [-1 0] symbol) \- \.)

       [x (+ y 2)]       \.
       [(inc x) (+ y 2)] (if (new-direction [0 -1] symbol) \| \.)
       [(+ x 2) (+ y 2)] \.})))


(defn state->string
  [state]
  (let [max-x (->> (keys state)
                   (reduce (fn [a [x _]]
                             (max a x))
                           0))]
    (->> (keys state)
         (sort (fn [c1 c2] (compare (into [] (reverse c1)) (into [] (reverse c2)))))
         (partition (inc max-x))
         (map (fn [row] (->> row
                             (mapv (fn [c] (str (get state c))))
                             (clojure.string/join "")))))))

(defn make-expanded-state
  {:test (fn []
           (is= (state->string (make-expanded-state test-input \|))
                ["............"
                 "....F-----7."
                 "....|.....|."
                 "....|.....|."
                 ".F--J.....S."
                 ".|........|."
                 ".|........|."
                 ".L--------J."
                 "............"]))}
  [input S-replacement]
  (->> input
       (map-indexed (fn [y row] [y row]))
       (reduce (fn [a [y row]]
                 (->> row
                      (map-indexed (fn [x symbol] [x symbol]))
                      (reduce (fn [a [x symbol]]
                                (let [s-symbol (= symbol \S)
                                      symbol (if s-symbol S-replacement symbol)
                                      data (create-expanded-data [x y] symbol)]
                                  (merge a (if s-symbol
                                             (assoc data [(inc (* 3 x)) (inc (* 3 y))] \S)
                                             data))))
                              a)))
               {})))

(comment
  (state->string (make-expanded-state test-input-b \F))
  )

(def directions [[-1 0] [1 0] [0 -1] [0 1]])

(defn outside-in-small-state?
  [small-loop-coordinates expanded-position]
  (let [small-position (map (fn [x] (quot x 3)) expanded-position)]
    (contains? small-loop-coordinates small-position)))

(defn enclosed-space
  {:test (fn []
           (is= (enclosed-space test-input \| [0 1])
                1)
           (is= (enclosed-space test-input-b \F [0 1])
                8)
           (is= (enclosed-space test-input-c \F [1 0])
                4))}
  [input replace-symbol start-direction]
  (let [small-state (create-state input)
        small-loop-coordinated (collect-loop-coordinates small-state {:current   (get-start-position small-state)
                                                                      :direction start-direction})
        expanded-start-position (->> (get-start-position small-state)
                                     (map (fn [x] (inc (* 3 x)))))
        expanded-state (make-expanded-state input replace-symbol)
        loop-coordinates (collect-loop-coordinates expanded-state
                                                   {:current   expanded-start-position
                                                    :direction start-direction})
        size-x (->> (keys expanded-state)
                    (map first)
                    (reduce (fn [a v] (max a v)))
                    (inc))
        size-y (->> (keys expanded-state)
                    (map second)
                    (reduce (fn [a v] (max a v)))
                    (inc))
        outside-positions (->> (loop [visited #{[0 0]}
                                      current-positions #{[0 0]}]
                                 (let [next-positions (->> current-positions
                                                           (reduce (fn [a p]
                                                                     (clojure.set/union a
                                                                                        (->> directions
                                                                                             (map (fn [d] (map + p d)))
                                                                                             (filter (fn [p]
                                                                                                       (and (not (contains? visited p))
                                                                                                            (<= 0 (first p) (dec size-x))
                                                                                                            (<= 0 (second p) (dec size-y))
                                                                                                            (not (contains? loop-coordinates p)))))
                                                                                             (into #{}))))
                                                                   #{}))]
                                   (if (empty? next-positions)
                                     visited
                                     (recur (clojure.set/union visited next-positions)
                                            next-positions)))))
        all-small-positions (/ (* size-y size-x) 9)
        small-number-of-loop-coordinates (/ (count loop-coordinates) 3)
        small-number-of-outside-positions (/ (->> outside-positions
                                                  (remove (fn [p] (outside-in-small-state? small-loop-coordinated p)))
                                                  (count))
                                             9)]
    (- all-small-positions
       small-number-of-loop-coordinates
       small-number-of-outside-positions)))

(deftest puzzle-b
  (is= (time (enclosed-space input \F [1 0]))
       ; "Elapsed time: 1186.639065 msecs"
       501))











