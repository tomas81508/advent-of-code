(ns advent-of-code.dec-2023.day-16
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is=]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]))

(def input (-> (slurp "src/advent_of_code/dec_2023/day_16_input.txt")
               (split-lines)))

(def test-input [".|...\\...."
                 "|.-.\\....."
                 ".....|-..."
                 "........|."
                 ".........."
                 ".........\\"
                 "..../.\\\\.."
                 ".-.-/..|.."
                 ".|....-|.\\"
                 "..//.|...."])

(def start-state {:currents [{:position [-1 0] :direction [1 0]}]
                  :visits   {}})

(defn v-add [v1 v2] (mapv + v1 v2))

(defn turn
  [input size position direction]
  (when (and (<= 0 (first position) (dec size))
             (<= 0 (second position) (dec size)))
    (case (get-in input (reverse position))
      \. [direction]
      \| (if (zero? (second direction)) [[0 1] [0 -1]] [direction])
      \- (if (zero? (first direction)) [[1 0] [-1 0]] [direction])
      \\ [({[1 0] [0 1], [0 1] [1 0], [-1 0] [0 -1], [0 -1] [-1 0]} direction)]
      \/ [({[1 0] [0 -1], [0 -1] [1 0], [-1 0] [0 1], [0 1] [-1 0]} direction)])))

(defn move-beam-a-step
  {:test (fn []
           (is= (move-beam-a-step test-input 10 start-state)
                {:visits   {}
                 :currents [{:position [0 0], :direction [1 0]}]})
           (is= (->> start-state
                     (move-beam-a-step test-input 10)
                     (move-beam-a-step test-input 10))
                {:currents [{:position [1 0] :direction [0 -1]}
                            {:position [1 0] :direction [0 1]}]
                 :visits   {[0 0] #{[1 0]}}}))}
  [input size state]
  (->> (:currents state)
       (remove (fn [{p :position d :direction}] (or (and (= -1 (first p)) (= d [-1 0]))
                                                    (and (= -1 (second p)) (= d [0 -1]))
                                                    (and (= size (first p)) (= d [1 0]))
                                                    (and (= size (second p)) (= d [0 1])))))
       (remove (fn [{p :position d :direction}] (contains? (get-in state [:visits p]) d)))
       (reduce (fn [state {p :position d :direction}]
                 (let [next-position (v-add p d)
                       next-directions (turn input size next-position d)]
                   (-> (if (and (<= 0 (first p) (dec size))
                                (<= 0 (second p) (dec size)))
                         (update-in state [:visits p] (fn [ds] (set (conj ds d))))
                         state)
                       (update :currents (fn [currents]
                                           (if (nil? next-directions)
                                             currents
                                             (apply conj currents (->> next-directions
                                                                       (map (fn [d] {:position  next-position
                                                                                     :direction d}))))))))))
               (select-keys state [:visits]))))

(defn move-until-the-end
  [input state]
  (let [size (count (first input))]
    (loop [state state]
      (if-not (:currents state)
        state
        (recur (move-beam-a-step input size state))))))

(defn state->string
  [input state]
  (->> (reduce-kv (fn [input [x y] directions]
                    (if (= (get-in input [y x]) \.)
                      (assoc-in input [y x]
                                (if (= 1 (count directions))
                                  ({[1 0] \> [-1 0] \< [0 1] \v [0 -1] \^} (first directions))
                                  (str (count directions))))
                      input))
                  (mapv (fn [l] (into [] (seq l))) input)
                  (:visits state))
       (map (fn [l] (clojure.string/join "" (map str l))))))

(defn energized-tiles
  [input start]
  (->> (move-until-the-end input {:currents [start]
                                  :visits   {}})
       (:visits)
       (keys)
       (count)))


(deftest puzzle-a
  (is= (time (energized-tiles input {:position [-1 0] :direction [1 0]}))
       ; "Elapsed time: 50.88781 msecs"
       6514)
  )

;; part 2

(deftest puzzle-b
  (is= (time (let [input input
                   size (count (first input))]
               (->> (concat (map (fn [x] {:position [x -1] :direction [0 1]}) (range size))
                            (map (fn [x] {:position [x size] :direction [0 -1]}) (range size))
                            (map (fn [y] {:position [-1 y] :direction [1 0]}) (range size))
                            (map (fn [y] {:position [size y] :direction [-1 0]}) (range size)))
                    (reduce (fn [a start]
                              (max a (energized-tiles input start)))
                            0))))
       ; "Elapsed time: 12412.500392 msecs"
       8089))








