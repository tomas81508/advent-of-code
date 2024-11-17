(ns advent-of-code.dec-2017.day-19
  (:require [ysera.test :refer [is=]]
            [clojure.string :refer [split-lines]]))

(def input (->> (slurp "src/advent_of_code/dec_2017/day_19_input.txt")
                (split-lines)))

(def test-input ["     |          "
                 "     |  +--+    "
                 "     A  |  C    "
                 " F---|----E|--+ "
                 "     |  |  |  D "
                 "     +B-+  +--+ "])

(defn create-configuration
  [input]
  (->> input
       (map-indexed (fn [y row] [y row]))
       (reduce (fn [configuration [y row]]
                 (->> row
                      (map-indexed (fn [x v] [x v]))
                      (reduce (fn [configuration [x v]]
                                (if (= v \space)
                                  configuration
                                  (assoc configuration [x y] v)))
                              configuration)))
               {})))

(def configuration (create-configuration input))
(def test-configuration (create-configuration test-input))

(def state {:position   [13 0]
            :direction  [0 1]
            :characters []})

(def test-state {:position   [5 0]
                 :direction  [0 1]
                 :characters []})

(defn walk-a-step
  [state configuration]
  (let [next-position (map + (:position state) (:direction state))
        value (get configuration next-position)]
    (cond (or (= value \|) (= value \-))
          (assoc state :position next-position)

          (= value \+)
          (-> state
              (assoc :position next-position)
              (assoc :direction (if (zero? (first (:direction state)))
                                  (let [left (map + next-position [-1 0])]
                                    (if (get configuration left) [-1 0] [1 0]))
                                  (let [down (map + next-position [0 1])]
                                    (if (get configuration down) [0 1] [0 -1])))))

          (not value)
          state

          :else
          (-> state
              (assoc :position next-position)
              (update :characters conj value)))))

(defn walk
  [state configuration]
  (loop [state state
         steps 1]
    (let [next-state (walk-a-step state configuration)]
      (if (= state next-state)
        {:state state :steps steps}
        (recur next-state
               (inc steps))))))

(comment
  (walk state configuration)

  )
