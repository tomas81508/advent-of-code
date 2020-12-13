(ns advent-of-code.dec-2020.day-12
  (:require [ysera.test :refer [is is-not is= deftest]]))

; Matrix lib (very inefficient but fun)

(defn scalar-product
  "The scalar product of two vectors"
  {:test (fn []
           (is= (scalar-product [1 4 0 -2] [5 -1 2 2])
                -3))}
  [v1 v2]
  (apply + (map * v1 v2)))

(defn transpose
  {:test (fn []
           (is= (transpose [[1 4]])
                [[1]
                 [4]])
           (is= (transpose [[1 21]
                            [3 4]])
                [[1 3]
                 [21 4]]))}
  [matrix]
  (reduce (fn [a [row-index row]]
            (reduce (fn [a [column-index v]]
                      (cond (not (contains? a column-index))
                            (assoc a column-index [v])

                            :else
                            (assoc-in a [column-index row-index] v)))
                    a
                    (map-indexed (fn [index v] [index v]) row)))
          []
          (map-indexed (fn [index row] [index row]) matrix)))

(defn matrix-multiplication
  {:test (fn []
           (is= (matrix-multiplication [[1 21]
                                        [3 4]
                                        [1 1]]
                                       [[0 -2 1]
                                        [4 0 1]])
                [[84 -2 22]
                 [16 -6 7]
                 [4 -2 2]])
           (is= (matrix-multiplication [[0 -1]
                                        [1 0]]
                                       [[1]
                                        [1]])
                [[-1]
                 [1]]))}
  [m1 m2]
  (let [m2-transposed (transpose m2)]
    (->> (for [row (range (count m1))
               column (range (count (first m2)))]
           [column row])
         (reduce (fn [a [column row]]
                   (assoc-in a [row column] (scalar-product (get m1 row) (get m2-transposed column))))
                 (vec (repeat (count m1) []))))))

(defn matrix-rotate-left
  {:test (fn []
           (is= (matrix-rotate-left [[1]
                                     [0]])
                [[0]
                 [1]]))}
  [v]
  (let [m [[0 -1] [1 0]]]
    (matrix-multiplication m v)))

(defn matrix-rotate-right
  {:test (fn []
           (is= (matrix-rotate-right [[10]
                                      [4]])
                [[4]
                 [-10]]))}
  [v]
  (let [m [[0 1] [-1 0]]]
    (matrix-multiplication m v)))


; -- Advent of code

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_12.txt")
       (clojure.string/split-lines)))

(def test-input ["F10" "N3" "F7" "R90" "F11"])

(defn create-state
  []
  {:position  [0 0]
   :direction [1 0]})


(defn left-turn-fn
  "Only works parallel to the axis."
  {:test (fn []
           (is= (left-turn-fn [2 0]) [0 2])
           (is= (left-turn-fn [0 1]) [-1 0])
           (is= (left-turn-fn [-10 0]) [0 -10])
           (is= (left-turn-fn [0 -1]) [1 0]))}
  [[x y]]
  (cond (pos? x) [0 x]
        (pos? y) [(- y) 0]
        (neg? x) [0 x]
        (neg? y) [(- y) 0]
        :else [0 0]))

(defn rotate-left
  {:test (fn []
           (is= (rotate-left [1 0] 90) [0 1])
           (is= (rotate-left [1 0] 180) [-1 0])
           (is= (rotate-left [1 0] 270) [0 -1])
           (is= (rotate-left [1 0] 360) [1 0]))}
  [d angle]
  (let [turns (/ angle 90)
        all-turns-fn (apply comp (repeat turns left-turn-fn))]
    (all-turns-fn d)))

(def right-turn-fn (apply comp (repeat 3 left-turn-fn)))

(defn rotate-right
  {:test (fn []
           (is= (rotate-right [1 0] 90) [0 -1])
           (is= (rotate-right [1 0] 180) [-1 0])
           (is= (rotate-right [1 0] 270) [0 1])
           (is= (rotate-right [1 0] 360) [1 0]))}
  [d angle]
  (let [turns (/ angle 90)
        all-turns-fn (apply comp (repeat turns right-turn-fn))]
    (all-turns-fn d)))

(defn move-an-instruction
  {:test (fn []
           (as-> (create-state) $
                 (move-an-instruction $ "F10")
                 (do (is= $ {:position  [10 0]
                             :direction [1 0]})
                     (move-an-instruction $ "N3"))
                 (do (is= $ {:position  [10 3]
                             :direction [1 0]})
                     (move-an-instruction $ "F7"))
                 (do (is= $ {:position  [17 3]
                             :direction [1 0]})
                     (move-an-instruction $ "R90"))
                 (do (is= $ {:position  [17 3]
                             :direction [0 -1]})
                     (move-an-instruction $ "F11"))
                 (is= $
                      {:position  [17 -8]
                       :direction [0 -1]})))}
  [state instruction]
  (let [[_ action units-as-string] (re-matches (re-pattern "(\\w)([\\d]+)") instruction)
        units (read-string units-as-string)]
    (case action
      "N" (update-in state [:position 1] (fn [x] (+ x units)))
      "S" (update-in state [:position 1] (fn [x] (- x units)))
      "E" (update-in state [:position 0] (fn [x] (+ x units)))
      "W" (update-in state [:position 0] (fn [x] (- x units)))
      "L" (update state :direction rotate-left units)
      "R" (update state :direction rotate-right units)
      "F" (update state :position (fn [p] (mapv + p (mapv (fn [x] (* x units)) (:direction state))))))))

(defn move-all-instructions
  [state instructions]
  (reduce move-an-instruction
          state
          instructions))

(defn abs [x] (if (pos? x) x (- x)))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [-1 1] [7 -15]) 24))}
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (apply +)))

(deftest puzzle-a
         (is= (->> (get-puzzle-input)
                   (move-all-instructions (create-state))
                   (:position)
                   (manhattan-distance [0 0]))
              1319))

(defn create-state-2 []
  {:position [0 0]
   :waypoint [10 1]})

(defn move-an-instruction-2
  {:test (fn []
           (as-> (create-state-2) $
                 (move-an-instruction-2 $ "F10")
                 (do (is= $ {:position [100 10]
                             :waypoint [10 1]})
                     (move-an-instruction-2 $ "N3"))
                 (do (is= $ {:position [100 10]
                             :waypoint [10 4]})
                     (move-an-instruction-2 $ "F7"))
                 (do (is= $ {:position [170 38]
                             :waypoint [10 4]})
                     (move-an-instruction-2 $ "R90"))
                 (do (is= $ {:position [170 38]
                             :waypoint [4 -10]})
                     (move-an-instruction-2 $ "F11"))
                 (is= $
                      {:position [214 -72]
                       :waypoint [4 -10]})))}
  [state instruction]
  (let [[_ action units-as-string] (re-matches (re-pattern "(\\w)([\\d]+)") instruction)
        units (read-string units-as-string)]
    (case action
      "N" (update-in state [:waypoint 1] (fn [x] (+ x units)))
      "S" (update-in state [:waypoint 1] (fn [x] (- x units)))
      "E" (update-in state [:waypoint 0] (fn [x] (+ x units)))
      "W" (update-in state [:waypoint 0] (fn [x] (- x units)))
      "L" (update state :waypoint (fn [[x y]]
                                    (let [turns (/ units 90)
                                          rotate-fn (apply comp (repeat turns matrix-rotate-left))
                                          [[x] [y]] (rotate-fn [[x] [y]])]
                                      [x y])))
      "R" (update state :waypoint (fn [[x y]]
                                    (let [turns (/ units 90)
                                          rotate-fn (apply comp (repeat turns matrix-rotate-right))
                                          [[x] [y]] (rotate-fn [[x] [y]])]
                                      [x y])))
      "F" (update state :position (fn [p]
                                    (let [movement (mapv (fn [x] (* x units)) (:waypoint state))]
                                      (mapv + p movement)))))))

(defn move-all-instructions-2
  [state instructions]
  (reduce move-an-instruction-2
          state
          instructions))

(deftest puzzle-b
         (is= (->> (get-puzzle-input)
                   (move-all-instructions-2 (create-state-2))
                   (:position)
                   (manhattan-distance [0 0]))
              62434))

