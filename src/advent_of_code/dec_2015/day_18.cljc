(ns advent-of-code.dec-2015.day-18
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.edn :as edn]))

(def input (-> (slurp "src/advent_of_code/dec_2015/day_18_input.txt")
               (clojure.string/split-lines)))

(def test-input [".#.#.#"
                 "...##."
                 "#....#"
                 "..#..."
                 "#.#..#"
                 "####.."])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:size   6
                 :lights #{[1 0] [2 3] [2 5] [5 4] [0 5] [3 0] [4 1] [5 2] [1 5] [2 4] [0 2] [0 4] [3 1] [5 0] [3 5]}}))}
  [input]
  {:size   (count input)
   :lights (->> input
                (reduce-kv (fn [a y x-row]
                             (->> x-row
                                  (vec)
                                  (reduce-kv (fn [a x c]
                                               (if (= c \#)
                                                 (conj a [x y])
                                                 a))
                                             a)))
                           #{}))})

(def directions (for [x (range -1 2 1)
                      y (range -1 2 1)
                      :when (not= [x y] [0 0])]
                  [x y]))

(defn should-turn-on?
  {:test (fn []
           ;; A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
           (is (should-turn-on? {:size 2 :lights #{[0 0] [1 0] [0 1]}} [0 0]))
           (is (should-turn-on? {:size 2 :lights #{[0 0] [1 0] [0 1] [1 1]}} [0 0]))
           (is-not (should-turn-on? {:size 3 :lights #{[0 0] [1 0] [0 1] [1 1] [2 2]}} [1 1]))
           ;; A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
           (is (should-turn-on? {:size 2 :lights #{[0 0] [1 0] [0 1]}} [1 1]))
           (is-not (should-turn-on? {:size 2 :lights #{[0 0] [1 0]}} [1 1])))}
  [state c]
  (let [on-neighbours (->> directions
                           (map (fn [d] (map + c d)))
                           (filter (fn [[x y]] (and (< -1 x (:size state))
                                                    (< -1 y (:size state))
                                                    (contains? (:lights state) [x y])))))]
    (or (and (contains? (:lights state) c)
             (< 1 (count on-neighbours) 4))
        (and (not (contains? (:lights state) c))
             (= (count on-neighbours) 3)))))

(defn step
  {:test (fn []
           (is= (step (create-state test-input))
                {:size   6
                 :lights #{[0 4] [0 5] [2 0] [2 1] [2 5] [3 0] [3 1] [3 2] [3 5] [4 2] [5 1]}}))}
  [state]
  (-> state
      (assoc :lights (->> (for [x (range (:size state))
                                y (range (:size state))]
                            [x y])
                          (filter (fn [c] (should-turn-on? state c)))
                          (into #{})))))

(comment
  (loop [state (create-state input)
         times 100]
    (if (zero? times)
      (count (:lights state))
      (recur (step state) (dec times))))
  )

; part 2

(defn create-state-2
  [input]
  (-> (create-state input)
      (update :lights (fn [lights]
                        (conj lights
                              [0 0]
                              [0 (dec (count input))]
                              [(dec (count input)) 0]
                              [(dec (count input)) (dec (count input))])))))

(defn should-turn-on-2?
  {:test (fn []
           ;; A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
           (is (should-turn-on-2? {:size 2 :lights #{[0 0] [1 0] [0 1]}} [0 0]))
           (is (should-turn-on-2? {:size 2 :lights #{[0 0] [1 0] [0 1] [1 1]}} [0 0]))
           (is-not (should-turn-on-2? {:size 3 :lights #{[0 0] [1 0] [0 1] [1 1] [2 2]}} [1 1]))
           ;; A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
           (is (should-turn-on-2? {:size 2 :lights #{[0 0] [1 0] [0 1]}} [1 1]))
           (is (should-turn-on-2? {:size 2 :lights #{[0 0] [1 0]}} [1 1])))}
  [state c]
  (let [on-neighbours (->> directions
                           (map (fn [d] (map + c d)))
                           (filter (fn [[x y]] (and (< -1 x (:size state))
                                                    (< -1 y (:size state))
                                                    (contains? (:lights state) [x y])))))]
    (or (contains? #{[0 0]
                     [0 (dec (:size state))]
                     [(dec (:size state)) 0]
                     [(dec (:size state)) (dec (:size state))]}
                   c)
        (and (contains? (:lights state) c)
             (< 1 (count on-neighbours) 4))
        (and (not (contains? (:lights state) c))
             (= (count on-neighbours) 3)))))

(defn step-2
  {:test (fn []
           (is= (step-2 (create-state-2 test-input))
                (create-state-2 ["#.##.#" "####.#" "...##." "......" "#...#." "#.####"])))}
  [state]
  (-> state
      (assoc :lights (->> (for [x (range (:size state))
                                y (range (:size state))]
                            [x y])
                          (filter (fn [c] (should-turn-on-2? state c)))
                          (into #{})))))

(comment
  (loop [state (create-state-2 input)
         times 100]
    (if (zero? times)
      (count (:lights state))
      (recur (step-2 state) (dec times))))

  )



