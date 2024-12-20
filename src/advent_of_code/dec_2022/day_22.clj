(ns advent-of-code.dec-2022.day-22
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :refer [split]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_22_input.txt")
                (clojure.string/split-lines)))

(def test-input ["        ...#"
                 "        .#.."
                 "        #..."
                 "        ...."
                 "...#.......#"
                 "........#..."
                 "..#....#...."
                 "..........#."
                 "        ...#...."
                 "        .....#.."
                 "        .#......"
                 "        ......#."
                 ""
                 "10R5L5R10L4R5L5"])

(defn create-path
  {:test (fn []
           (is= (create-path "10R5L5R10L4R5L5")
                [10 \R 5 \L 5 \R 10 \L 4 \R 5 \L 5]))}
  [input]
  (->> (split input #"L")
       (map (fn [s] (->> (split s #"R")
                         (map read-string)
                         (interpose \R))))
       (interpose \L)
       (flatten)))

(defn create-map
  {:test (fn []
           (is= (create-map ["  ...#"
                             "  .#.."
                             ".#..."
                             "...#."])
                {:walls      #{[6 1] [4 2] [2 3] [4 4]}
                 :open-tiles #{[3 1] [4 1] [5 1]
                               [3 2] [5 2] [6 2]
                               [1 3] [3 3] [4 3] [5 3]
                               [1 4] [2 4] [3 4] [5 4]}}))}
  [input-lines]
  (->> input-lines
       (map-indexed (fn [index line] [index line]))
       (reduce (fn [a [y rows]]
                 (->> rows
                      (map-indexed (fn [index row] [index row]))
                      (reduce (fn [a [x c]]
                                (condp = c
                                  \space a
                                  \. (update a :open-tiles conj [(inc x) (inc y)])
                                  \# (update a :walls conj [(inc x) (inc y)])))
                              a)))
               {:walls #{} :open-tiles #{}})))

(declare find-min-x)

(defn find-boundary
  [state min-or-max first-or-second value]
  (let [other (if (= first-or-second first) second first)]
    (->> (clojure.set/union (get-in state [:map :open-tiles])
                            (get-in state [:map :walls]))
         (keep (fn [c] (when (= (other c) value) (first-or-second c))))
         (apply min-or-max))))

(defn find-min-x [state y] (find-boundary state min first y))
(defn find-max-x [state y] (find-boundary state max first y))
(defn find-min-y [state x] (find-boundary state min second x))
(defn find-max-y [state x] (find-boundary state max second x))

(defn create-state
  [input]
  (let [state {:map    (create-map (drop-last 2 input))
               :path   (create-path (last input))
               :facing :east}]
    (assoc state :position [(find-min-x state 1) 1])))

(def test-state (create-state test-input))

(def state (create-state input))

(def facing->vector {:east [1 0] :south [0 1] :west [-1 0] :north [0 -1]})

(defn strategy-2d
  [state position]
  {:position (condp = (:facing state)
               :east [(find-min-x state (second position)) (second position)]
               :west [(find-max-x state (second position)) (second position)]
               :north [(first position) (find-max-y state (first position))]
               :south [(first position) (find-min-y state (first position))])
   :facing   (:facing state)})

(defn move
  {:test (fn []
           (is= (-> (move test-state strategy-2d)
                    (:position))
                [11 1])
           (is= (-> (move test-state strategy-2d)
                    (move strategy-2d)
                    (:facing))
                :south)
           )}
  [state strategy]
  (let [instruction (first (:path state))
        walls (get-in state [:map :walls])
        open-tiles (get-in state [:map :open-tiles])]
    (-> (cond (= instruction \L)
              (update state :facing {:east :north :north :west :west :south :south :east})

              (= instruction \R)
              (update state :facing {:east :south :south :west :west :north :north :east})

              :else
              (loop [steps instruction
                     position (:position state)
                     facing (:facing state)]
                (let [direction (facing->vector facing)]
                  (if (zero? steps)
                    (assoc state :position position :facing facing)
                    (let [next-position (map + position direction)]
                      (cond (contains? walls next-position)
                            (assoc state :position position :facing facing)

                            (contains? open-tiles next-position)
                            (recur (dec steps) next-position facing)

                            :outside-of-map
                            (let [{next-position :position facing :facing} (strategy state next-position)]
                              (if (contains? walls next-position)
                                (assoc state :position position)
                                (recur (dec steps) next-position facing)))))))))
        (update :path (partial drop 1)))))

(defn move-full-path
  {:test (fn []
           (is= (-> (move-full-path test-state strategy-2d)
                    (:position))
                [8 6]))}
  [state strategy]
  (loop [state state]
    (if (empty? (:path state))
      state
      (recur (move state strategy)))))

(defn calculate-password
  {:test (fn []
           (is= (calculate-password [8 6] :east)
                6032))}
  [position facing]
  (+ (* 1000 (second position))
     (* 4 (first position))
     ({:east 0 :south 1 :west 2 :north 3} facing)))

(comment
  (time (as-> state $
              (move-full-path $ strategy-2d)
              (calculate-password (:position $) (:facing $))))
  ; "Elapsed time: 476.739456 msecs"
  ; => 136054
  )

; My puzzle structure
;      F  G
;     ######
;    E######B
;     ######
;     ### A
;    D###A
;   D ###
;  ######
; E######B
;  ######
;  ###
; F###C
;  ###
;   G

(defn puzzle-strategy-3d
  [state [x y]]
  (let [facing (:facing state)]
    (cond
      ; F
      (and (zero? y) (<= 51 x 100)) {:position [1 (+ 100 x)] :facing :east}
      (and (zero? x) (<= 151 y)) {:position [(- y 100) 1] :facing :south}
      ; G
      (and (zero? y) (<= 101 x)) {:position [(- x 100) 200] :facing :north}
      (= y 201) {:position [(+ 100 x) 1] :facing :south}
      ; E
      (and (= x 50) (<= y 50)) {:position [1 (- 151 y)] :facing :east}
      (and (zero? x) (<= 101 y 150)) {:position [51 (- 151 y)] :facing :east}
      ; A
      (and (= y 51) (<= 101 x) (= facing :south)) {:position [100 (- x 50)] :facing :west}
      (and (= x 101) (<= 51 y 100) (= facing :east)) {:position [(+ y 50) 50] :facing :north}
      ; B
      (= x 151) {:position [100 (- 151 y)] :facing :west}
      (and (= x 101) (<= 101 y 150)) {:position [150 (- 151 y)] :facing :west}
      ; C
      (and (= x 51) (= facing :east)) {:position [(- y 100) 150] :facing :north}
      (and (= y 151) (= facing :south)) {:position [50 (+ 100 x)] :facing :west}
      ; D
      (and (= 50 x) (<= 51 y 100) (= facing :west)) {:position [(- y 50) 101] :facing :south}
      (and (= y 100) (= facing :north)) {:position [51 (+ 50 x)] :facing :east}

      :else
      "This should never happen")))

(comment

  (time (as-> state $
              (move-full-path $ puzzle-strategy-3d)
              (calculate-password (:position $) (:facing $))))
  ; "Elapsed time: 25.822908 msecs"
  ; => 122153
  )
