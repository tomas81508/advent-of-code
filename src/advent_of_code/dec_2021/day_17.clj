(ns advent-of-code.dec-2021.day-17
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def puzzle-input {:x [144 178] :y [-100 -76]})

(defn step
  {:test (fn []
           (is= (step {:position [0 0]
                       :velocity [7 2]})
                {:position [7 2]
                 :velocity [6 1]}))}
  [state]
  (-> state
      (update :position (fn [position] (map + position (:velocity state))))
      (update :velocity (fn [velocity]
                          (let [[x y] velocity]
                            (map + velocity [(cond (pos? x) -1 (zero? x) 0 :else 1) -1]))))))

(defn in-target?
  {:test (fn []
           (is (in-target? {:position [10 10]} {:x [0 20] :y [8 12]}))
           (is-not (in-target? {:position [100 10]} {:x [0 20] :y [8 12]})))}
  [{position :position} {x :x y :y}]
  (and (<= (first x) (first position) (second x))
       (<= (first y) (second position) (second y))))

(defn miss?
  {:test (fn []
           (is (miss? {:position [0 0] :velocity [5 0]} {:x [0 20] :y [8 12]}))
           (is (miss? {:position [40 10] :velocity [0 10]} {:x [2 20] :y [0 5]}))
           (is-not (miss? {:position [0 10] :velocity [5 5]} {:x [0 20] :y [0 5]})))}
  [{[px py] :position [vx vy] :velocity} {[min-x max-x] :x [min-y max-y] :y}]
  (or (< py min-y) (> px max-x)))

(defn launch
  {:test (fn []
           (is= (launch {:position [0 0] :velocity [7 2]}
                        {:x [20 30] :y [-10 -5]})
                3)
           (is= (launch {:position [0 0] :velocity [6 3]}
                        {:x [20 30] :y [-10 -5]})
                6)
           (is= (launch {:position [0 0] :velocity [9 0]}
                        {:x [20 30] :y [-10 -5]})
                0)
           (is= (launch {:position [0 0] :velocity [6 9]}
                        {:x [20 30] :y [-10 -5]})
                45)
           (is-not (launch {:position [0 0] :velocity [17 -4]}
                           {:x [20 30] :y [-10 -5]})))}
  [state target]
  (loop [state state
         max-y 0]
    (cond (in-target? state target) max-y
          (miss? state target) nil
          :else (let [state (step state)]
                  (recur state (max max-y (second (:position state))))))))

(comment
  (->> (for [x (range 144)
             y (range -100 100)]
         [x y])
       (keep (fn [velocity]
               (launch {:position [0 0] :velocity velocity} puzzle-input)))
       (apply max))
  ; => 4950

  (->> (for [x (range 180)
             y (range -100 300)]
         [x y])
       (filter (fn [velocity]
                 (launch {:position [0 0] :velocity velocity} puzzle-input)))
       (count))
  )
