(ns advent-of-code.dec-2022.day-14
  (:require [ysera.test :refer [is is-not is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_14_input.txt")
                (clojure.string/split-lines)))

(def test-input ["498,4 -> 498,6 -> 496,6"
                 "503,4 -> 502,4 -> 502,9 -> 494,9"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:rocks        #{[502 9] [494 9] [496 9] [503 4] [497 6] [498 5] [502 7] [495 9] [496 6] [500 9] [501 9] [502 8] [502 6] [498 9] [502 5] [499 9] [498 6] [497 9] [502 4] [498 4]}
                 :sand         [500 0]
                 :sand-at-rest #{}}))}
  [input]
  {:sand-at-rest #{}
   :sand         [500 0]
   :rocks        (->> input
                      (map (fn [row]
                             (->> (clojure.string/split row #" -> ")
                                  (map (fn [edge]
                                         (->> (clojure.string/split edge #",")
                                              (map read-string))))
                                  (partition 2 1)
                                  (map (fn [[[x1 y1] [x2 y2]]]
                                         (for [x (range (min x1 x2) (inc (max x1 x2)))
                                               y (range (min y1 y2) (inc (max y1 y2)))]
                                           [x y])))
                                  (apply concat)
                                  (set))))
                      (reduce clojure.set/union))})

(defn free-space?
  [state position]
  (and (not (contains? (:sand-at-rest state) position))
       (not (contains? (:rocks state) position))))

(defn let-sand-fall
  {:test (fn []
           (is= (-> (create-state test-input)
                    (let-sand-fall)
                    (:sand-at-rest)
                    (count))
                24))}
  [state]
  (let [max-y (->> (:rocks state)
                   (map second)
                   (apply max))]
    (loop [state state]
      (let [down-position (map + (:sand state) [0 1])]
        (cond (> (second down-position) max-y)
              state

              (free-space? state down-position)
              (recur (assoc state :sand down-position))

              (let [down-left (map + down-position [-1 0])]
                (free-space? state down-left))
              (recur (assoc state :sand (map + down-position [-1 0])))

              (let [down-right (map + down-position [1 0])]
                (free-space? state down-right))
              (recur (assoc state :sand (map + down-position [1 0])))

              :else
              (recur (-> state
                         (update :sand-at-rest conj (:sand state))
                         (assoc :sand [500 0]))))))))

(comment
  (time
    (-> (create-state input)
        (let-sand-fall)
        (:sand-at-rest)
        (count)))
  ; "Elapsed time: 145.611656 msecs"
  ; => 843
  )

(defn let-sand-fall-to-the-ground
  {:test (fn []
           (is= (-> (create-state test-input)
                    (let-sand-fall-to-the-ground)
                    (:sand-at-rest)
                    (count))
                93))}
  [state]
  (let [ground (->> (:rocks state)
                    (map second)
                    (apply max)
                    (+ 2))]
    (loop [state state]
      (let [down-position (map + (:sand state) [0 1])]
        (cond (= (second down-position) ground)
              (recur (-> state
                         (update :sand-at-rest conj (:sand state))
                         (assoc :sand [500 0])))

              (free-space? state down-position)
              (recur (assoc state :sand down-position))

              (let [down-left (map + down-position [-1 0])]
                (free-space? state down-left))
              (recur (assoc state :sand (map + down-position [-1 0])))

              (let [down-right (map + down-position [1 0])]
                (free-space? state down-right))
              (recur (assoc state :sand (map + down-position [1 0])))

              (= down-position [500 1])
              (update state :sand-at-rest conj (:sand state))

              :else
              (recur (-> state
                         (update :sand-at-rest conj (:sand state))
                         (assoc :sand [500 0]))))))))

(comment
  (time
    (-> (create-state input)
        (let-sand-fall-to-the-ground)
        (:sand-at-rest)
        (count)))
  ; "Elapsed time: 4667.057478 msecs"
  ; => 27625
  )