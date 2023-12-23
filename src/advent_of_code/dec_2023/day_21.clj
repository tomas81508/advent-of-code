(ns advent-of-code.dec-2023.day-21
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]))

(def input (-> (slurp "src/advent_of_code/dec_2023/day_21_input.txt")
               (clojure.string/split-lines)))
(def test-input ["..........."
                 ".....###.#."
                 ".###.##..#."
                 "..#.#...#.."
                 "....#.#...."
                 ".##..S####."
                 ".##..#...#."
                 ".......##.."
                 ".##.#.####."
                 ".##..##.##."
                 "..........."])

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn create-data
  [input]
  (reduce-kv (fn [a y row]
               (reduce-kv (fn [a x c]
                            (case c
                              \S (assoc a :current #{[x y]})
                              \# (update a :rocks conj [x y])
                              a))
                          a
                          (vec (seq row))))
             {:current #{}
              :rocks   #{}
              :size    (count input)}
             input))

(def test-data (create-data test-input))
(def test-state (select-keys test-data [:current]))
(def test-garden-map (select-keys test-data [:rocks :size]))

(def data (create-data input))
(def state (select-keys data [:current]))
(def garden-map (select-keys data [:rocks :size]))

(defn rock? [garden-map p] (contains? (:rocks garden-map) p))
(defn outside-the-garden? [{size :size} [x y]] (not (and (<= 0 x (dec size)) (<= 0 y (dec size)))))

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-garden-map test-state)
                {:current #{[4 5] [5 4]}})
           (is= (walk-a-step test-garden-map {:current #{[4 5] [5 4]}})
                {:current #{[5 3] [4 6] [5 5] [3 5]}})
           (is= (walk-a-step garden-map state)
                {:current #{[64 65] [66 65] [65 66] [65 64]}}))}
  [garden-map state]
  (reduce (fn [state p]
            (reduce (fn [state d]
                      (let [np (map + p d)]
                        (if (or (rock? garden-map np)
                                (outside-the-garden? garden-map np))
                          state
                          (update state :current conj np))))
                    state
                    directions))
          (-> state
              (assoc :current #{}))
          (:current state)))

(defn walk-n-steps
  {:test (fn []
           (is= (-> (walk-n-steps test-garden-map test-state 6)
                    (:current)
                    (count))
                16))}
  [garden-map state n]
  (reduce (fn [state _]
            (walk-a-step garden-map state))
          state
          (range n)))

(deftest puzzle-a
  (is= (time (-> (walk-n-steps garden-map state 64)
                 (:current)
                 (count)))
       3788))

; part 2

(def steps 26501365)

(defn count-walk
  [garden-map start-position steps]
  (-> (walk-n-steps garden-map {:current #{start-position}} steps)
      (:current)
      (count)))

(defn solve-b
  [garden-map steps]
  (let [size (:size garden-map)
        middle-position (/ (dec size) 2)
        max-position (dec size)
        full-garden-maps (quot steps size)
        reminder (rem steps size)]
    {:west-corner  (count-walk garden-map [max-position middle-position] size)
     :north-corner (count-walk garden-map [middle-position max-position] size)
     :east-corner  (count-walk garden-map [0 middle-position] size)
     :south-corner (count-walk garden-map [middle-position 0] size)
     :full-garden  (count-walk garden-map [middle-position middle-position] (+ (* 2 size)
                                                                               (if (odd? steps) 1 0)))}
    ))

(comment
  (solve-b garden-map steps)

  (def size (:size garden-map))
  (def full-garden-maps (quot steps size))
  (def the-rest (rem steps size))

  (def west-corner (count-walk garden-map [130 65] 130))
  (def north-corner (count-walk garden-map [65 130] 130))
  (def east-corner (count-walk garden-map [0 65] 130))
  (def south-corner (count-walk garden-map [65 0] 130))

  (def full-garden-even (count-walk garden-map [65 65] 150))
  (def full-garden-odd (count-walk garden-map [65 65] 151))
  (def number-of-even-full-gardens (* full-garden-maps full-garden-maps))
  (def number-of-odd-full-gardens (* (dec full-garden-maps)
                                     (dec full-garden-maps)))

  (def large-north-west (count-walk garden-map [130 130] 195))
  (def large-north-east (count-walk garden-map [0 130] 195))
  (def large-south-west (count-walk garden-map [130 0] 195))
  (def large-south-east (count-walk garden-map [0 0] 195))

  (def small-north-west (count-walk garden-map [130 130] 64))
  (def small-north-east (count-walk garden-map [0 130] 64))
  (def small-south-west (count-walk garden-map [130 0] 64))
  (def small-south-east (count-walk garden-map [0 0] 64))

  (def solution
    (+ (* number-of-even-full-gardens full-garden-even)
       (* number-of-odd-full-gardens full-garden-odd)
       west-corner north-corner east-corner south-corner
       (* (dec full-garden-maps) (+ large-north-east
                                    large-north-west
                                    large-south-east
                                    large-south-west))
       (* full-garden-maps (+ small-north-east
                              small-north-west
                              small-south-east
                              small-south-west))))
;      solution 631357596621921
  )





