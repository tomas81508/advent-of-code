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
