(ns advent-of-code.dec-2022.day-12
  (:require [clojure.test :refer [deftest]]
            [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/dec_2022/day_12_input.txt"))

(def test-input (str "Sabqponm\n"
                     "abcryxxl\n"
                     "accszExk\n"
                     "acctuvwj\n"
                     "abdefghi"))

(defn input->map [input]
  (->> input
       (clojure.string/split-lines)
       (map-indexed (fn [y xs] (map-indexed (fn [x height] [[x y] height]) xs)))
       (apply concat)
       (reduce conj {})))

(def test-map (input->map test-input))

(def input-map (input->map input))



(def test-state {:currents #{[0 0]}
                 :length   0
                 :visits   {[0 0] 0}
                 :goal     [5 2]})

(def state {:currents #{[0 20]}
            :length   0
            :visits   {[0 20] 0}
            :goal     [40 20]})

(defn distance
  {:test (fn []
           (is= (distance [0 0] [4 3]) 5.0))}
  [from to]
  (->> (map - from to)
       (map (fn [x] (* x x)))
       (reduce +)
       (Math/sqrt)))


(defn close-in-height?
  {:test (fn []
           (is (close-in-height? \a \b))
           (is (close-in-height? \a \S))
           (is (close-in-height? \a \a))
           (is-not (close-in-height? \a \E)))}
  [from to]
  (let [from (condp = from \S \a \E \z from)
        to (condp = to \S \a \E \z to)]
    (<= (- (int to) (int from)) 1)))


(defn can-walk?
  {:test (fn []
           (is (can-walk? test-map [0 0] [1 0]))
           (is-not (can-walk? test-map [8 0] [9 0]))
           (is-not (can-walk? test-map [2 0] [3 0]))
           (is-not (can-walk? test-map [0 0] [2 0])))}
  [the-map from to]
  (let [height-from (get the-map from)
        height-to (get the-map to)]
    (and height-from
         height-to
         (= (distance from to) 1.0)
         (close-in-height? height-from height-to))))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn walk-one-step
  {:test (fn []
           (is= (walk-one-step test-state test-map)
                {:currents #{[0 1] [1 0]}
                 :length   1
                 :visits   {[0 0] 0
                            [1 0] 1
                            [0 1] 1}
                 :goal     [5 2]})
           (is= (-> test-state
                    (walk-one-step test-map)
                    (walk-one-step test-map))
                {:currents #{[0 2] [2 0] [1 1]}
                 :length   2
                 :visits   {[0 0] 0, [1 0] 1, [0 1] 1, [2 0] 2, [1 1] 2, [0 2] 2}
                 :goal     [5 2]}))}
  [state the-map]
  (reduce (fn [state position]
            (reduce (fn [state direction]
                      (let [to-position (map + position direction)]
                        (if (or (not (can-walk? the-map position to-position))
                                (contains? (:visits state) to-position))
                          state
                          (-> state
                              (update :currents conj to-position)
                              (assoc-in [:visits to-position] (:length state))))))
                    state
                    directions))
          (-> state
              (assoc :currents #{})
              (update :length inc))
          (:currents state)))

(comment
  (time
    (-> (loop [state state]
          (let [state (walk-one-step state input-map)]
            (println (:currents state))
            (println (:length state))
            (if (or (contains? (:visits state) (:goal state))
                    (empty? (:currents state)))
              state
              (recur state))))
        (:visits)
        (get (:goal state))))
  ; "Elapsed time: 54.572577 msecs"
  ; => 370
  )

(def b-state {:currents #{[40 20]}
              :length   0
              :visits   {[40 20] 0}})


(defn walk-backwards-one-step
  {:test (fn []
           (is= (walk-backwards-one-step b-state input-map)
                {:currents #{[41 20]}
                 :length   1
                 :visits   {[40 20] 0
                            [41 20] 1}}))}
  [state the-map]
  (reduce (fn [state position]
            (reduce (fn [state direction]
                      (let [from-position (map + position direction)]
                        (if (or (not (can-walk? the-map from-position position))
                                (contains? (:visits state) from-position))
                          state
                          (-> state
                              (update :currents conj from-position)
                              (assoc-in [:visits from-position] (:length state))))))
                    state
                    directions))
          (-> state
              (assoc :currents #{})
              (update :length inc))
          (:currents state)))

(comment
  (time
    (-> (loop [state b-state]
          (let [state (walk-backwards-one-step state input-map)]
            (println (:length state))
            (if (or (->> (:currents state)
                         (some (fn [c]
                                 (= (get input-map c) \a))))
                    (empty? (:currents state)))
              state
              (recur state))))
        (:length)))
  ; "Elapsed time: 54.572577 msecs"
  ; => nil
  )


