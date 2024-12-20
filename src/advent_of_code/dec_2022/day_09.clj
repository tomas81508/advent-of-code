(ns advent-of-code.dec-2022.day-09
  (:require [advent-of-code.test :refer [is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_09_input.txt")
                (clojure.string/split-lines)))

(def test-input ["R 4" "U 4" "L 3" "D 1" "R 4" "D 1" "L 5" "R 2"])

(def directions {"L" [-1 0]
                 "D" [0 -1]
                 "R" [1 0]
                 "U" [0 1]})

(def state {:head    [0 0]
            :tail    [0 0]
            :visited #{[0 0]}})

(defn get-instructions
  [input]
  (->> input
       (map (fn [instruction]
              (let [result (clojure.string/split instruction #" ")]
                {:direction (directions (first result))
                 :amount    (read-string (second result))})))))

(defn move-towards-head
  {:test (fn []
           (is= (move-towards-head [4 4] [2 2])
                [3 3])
           (is= (move-towards-head [2 0] [0 0])
                [1 0]))}
  [[hx hy] [tx ty :as tail]]
  (let [dx (- hx tx)
        dy (- hy ty)]
    (if (<= (max (abs dx) (abs dy)) 1)
      tail
      (let [mx (cond (pos? dx) 1 (neg? dx) -1 :else 0)
            my (cond (pos? dy) 1 (neg? dy) -1 :else 0)]
        [(+ tx mx) (+ ty my)]))))

(defn move
  {:test (fn []
           (is= (-> (move state test-input)
                    (:visited)
                    (count))
                13))}
  [state input]
  (->> (reduce (fn [state {d :direction a :amount}]
                 (reduce (fn [state _]
                           (let [head-next-position (map + (:head state) d)
                                 tail-next-position (move-towards-head head-next-position (:tail state))]
                             (-> state
                                 (assoc :head head-next-position)
                                 (assoc :tail tail-next-position)
                                 (update :visited conj tail-next-position))))
                         state
                         (range a)))
               state
               (get-instructions input))))

(comment
  (time (-> (move state input)
            (:visited)
            (count)))
  ; "Elapsed time: 24.149976 msecs"
  ; => 6311
  )

(def larger-input ["R 5" "U 8" "L 8" "D 3" "R 17" "D 10" "L 25" "U 20"])

(def state2 {:rope    (vec (repeat 10 [0 0]))
             :visited #{[0 0]}})

(defn move-with-tail
  {:test (fn []
           (is= (-> (move-with-tail state2 test-input)
                    (:visited)
                    (count))
                1)
           (is= (-> (move-with-tail state2 larger-input)
                    (:visited)
                    (count))
                36)
           )}
  [state input]
  (->> (reduce (fn [state {d :direction a :amount}]
                 (reduce (fn [state _]
                           (let [head-next-position (map + (first (:rope state)) d)]
                             (->> (:rope state)
                                  (map-indexed (fn [index item] [index item]))
                                  (drop 1)
                                  ; index will now start from 1
                                  (reduce (fn [state [index p]]
                                            (let [local-head (get (:rope state) (dec index))
                                                  new-p (move-towards-head local-head p)]
                                              (-> (if (= index 9)
                                                    (update state :visited conj new-p)
                                                    state)
                                                  (assoc-in [:rope index] new-p))))
                                          (assoc-in state [:rope 0] head-next-position)))))
                         state
                         (range a)))
               state
               (get-instructions input))))

(comment
  (time (-> (move-with-tail state2 input)
            (:visited)
            (count)))
  ; "Elapsed time: 86.682643 msecs"
  ; => 2482
  )




