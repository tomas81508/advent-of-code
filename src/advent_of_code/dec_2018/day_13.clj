(ns advent-of-code.dec-2018.day-13
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))


(def input (->> (slurp "src/advent_of_code/dec_2018/day_13.txt")
                (string/split-lines)))

(def test-input ["/->-\\        "
                 "|   |  /----\\"
                 "| /-+--+-\\  |"
                 "| | |  | v  |"
                 "\\-+-/  \\-+--/"
                 "  \\------/ "])

(defn find-cars
  {:test (fn []
           (is= (find-cars test-input)
                [{:position  [2 0]
                  :direction [1 0]
                  :next-turn :left}
                 {:position  [9 3]
                  :direction [0 1]
                  :next-turn :left}]))}
  [input]
  (->> input
       (reduce-kv (fn [cars y line]
                    (->> (vec (seq line))
                         (reduce-kv (fn [cars x c]
                                      (let [car (case c
                                                  \^ {:position [x y] :direction [0 -1] :next-turn :left}
                                                  \> {:position [x y] :direction [1 0] :next-turn :left}
                                                  \v {:position [x y] :direction [0 1] :next-turn :left}
                                                  \< {:position [x y] :direction [-1 0] :next-turn :left}
                                                  nil)]
                                        (if car (conj cars car) cars)))
                                    cars)))
                  [])))

(defn get-road-piece
  {:test (fn []
           (is= (get-road-piece test-input [9 3]) \v))}
  [input [x y]]
  (get-in input [y x]))

(def test-cars (find-cars test-input))
(def cars (find-cars input))

(defn colliding-cars?
  [cars]
  (let [number-of-cars (count cars)]
    (not= number-of-cars
          (->> cars
               (map :position)
               (distinct)
               (count)))))

(def straight #{\| \- \v \> \< \^})

(def turn-left {[1 0] [0 -1] [0 -1] [-1 0] [-1 0] [0 1] [0 1] [1 0]})
(def turn-right {[1 0] [0 1] [0 1] [-1 0] [-1 0] [0 -1] [0 -1] [1 0]})

(defn move-car
  {:test (fn []
           (is= (move-car test-input {:position [9 3] :direction [0 1] :next-turn :left})
                {:position [9 4] :direction [1 0] :next-turn :straight})
           (is= (move-car test-input {:position [3 0] :direction [1 0] :next-turn :left})
                {:position [4 0] :direction [0 1] :next-turn :left}))}
  [input {position :position direction :direction next-turn :next-turn :as car}]
  (let [new-position (map + position direction)
        road-piece (get-road-piece input new-position)]
    (cond (contains? straight road-piece)
          (assoc car :position new-position)

          (= road-piece \\)
          (assoc car :position new-position
                     :direction (case direction [1 0] [0 1] [0 1] [1 0] [-1 0] [0 -1] [0 -1] [-1 0]))

          (= road-piece \/)
          (assoc car :position new-position
                     :direction (case direction [1 0] [0 -1] [0 1] [-1 0] [-1 0] [0 1] [0 -1] [1 0]))

          (= road-piece \+)
          (case next-turn
            :left {:position new-position :direction (turn-left direction) :next-turn :straight}
            :straight {:position new-position :direction direction :next-turn :right}
            :right {:position new-position :direction (turn-right direction) :next-turn :left}))))

(defn move
  {:test (fn []
           (is= (move test-input test-cars)
                [{:position [3 0] :direction [1 0] :next-turn :left}
                 {:position [9 4] :direction [1 0] :next-turn :straight}])
           (is= (move test-input [{:position [3 0] :direction [1 0] :next-turn :left} {:position [9 4] :direction [1 0] :next-turn :straight}])
                [{:position [4 0] :direction [0 1] :next-turn :left} {:position [10 4] :direction [1 0] :next-turn :straight}]))}
  [input cars]
  (loop [[c & r] cars
         new-cars []]
    (let [new-car (move-car input c)
          new-cars (conj new-cars new-car)]
      (cond (colliding-cars? (concat new-cars r)) {:collision (concat new-cars r)}
            r (recur r new-cars)
            :else new-cars))))

(defn get-collision-coordinate
  [cars]
  (->> cars
       (map :position)
       (frequencies)
       (seq)
       (some (fn [[x f]] (when (= f 2) x)))))

(defn move-until-collision
  {:test (fn []
           (is= (move-until-collision test-input test-cars)
                [7 3]))}
  [input cars]
  (loop [cars cars]
    (let [cars (move input cars)]
      (if (:collision cars)
        (get-collision-coordinate (:collision cars))
        (recur (sort-by (fn [{[x y] :position}] [y x]) cars))))))

(defn puzzle-1
  []
  (move-until-collision input cars))

(comment
  (time (puzzle-1))
  )

(def test-input-2 ["/>-<\\  "
                   "|   |  "
                   "| /<+-\\"
                   "| | | v"
                   "\\>+</ |"
                   "  |   ^"
                   "  \\<->/"])

(def test-cars-2 (find-cars test-input-2))

(defn move-and-clean
  {:test (fn []
           (is= (move-and-clean test-input-2 test-cars-2)
                [{:position [6 6] :direction [0 -1] :next-turn :left}
                 {:position [2 6] :direction [0 -1] :next-turn :left}
                 {:position [2 2] :direction [0 1] :next-turn :left}]))}
  [input cars]
  (loop [[c & r] cars
         new-cars []]
    (if-not c
      new-cars
      (let [new-car (move-car input c)
            new-cars (conj new-cars new-car)]
        (if (colliding-cars? (concat new-cars r))
          (let [coordinate (get-collision-coordinate (concat new-cars r))]
            (recur (remove (fn [{position :position}] (= position coordinate)) r)
                   (remove (fn [{position :position}] (= position coordinate)) new-cars)))
          (recur r new-cars))))))


(defn move-and-clean-until-the-end
  {:test (fn []
           (is= (move-and-clean-until-the-end test-input-2 test-cars-2)
                [6 4])
           )}
  [input cars]
  (loop [cars cars]
    (let [cars (move-and-clean input cars)]
      (if (= (count cars) 1)
        (:position (first cars))
        (recur (sort-by (fn [{[x y] :position}] [y x]) cars))))))

(comment
  (time (move-and-clean-until-the-end input cars))
  )


