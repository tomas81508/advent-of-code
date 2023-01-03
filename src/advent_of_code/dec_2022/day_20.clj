(ns advent-of-code.dec-2022.day-20
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_20_input.txt")
                (clojure.string/split-lines)
                (mapv read-string)))

(defn create-state [input]
  (let [last-index (dec (count input))]
    (->> input
         (reduce-kv (fn [a index v]
                      (conj a {:id       index
                               :value    v
                               :previous (if (zero? index) last-index (dec index))
                               :next     (if (= index last-index) 0 (inc index))}))
                    []))))

(def state (create-state input))
(def test-state (create-state [1 2 -3 3 -2 0 4]))

(defn to-vector [state]
  (let [length (count state)]
    (loop [i 0
           element (get state 0)
           result []]
      (if (= i length)
        result
        (recur (inc i) (get state (:next element)) (conj result (:value element)))))))

(defn move
  {:test (fn []
           (is= (-> (create-state [2 -3 3 1])
                    (move 1)
                    (to-vector))
                [2 -3 3 1])
           (is= (-> (create-state [1 2 -3 3 -2 0 4])
                    (move 0)
                    (to-vector))
                [1 -3 3 -2 0 4 2])
           (is= (-> (create-state [1 2 -3 3 -2 0 4])
                    (move 0)
                    (move 1)
                    (to-vector))
                [1 -3 2 3 -2 0 4])
           (is= (-> (create-state [1 2 -3 3 -2 0 4])
                    (move 0)
                    (move 1)
                    (move 2)
                    (to-vector))
                [1 2 3 -2 -3 0 4])
           (is= (-> (create-state [1 2 -3 3 -2 0 4])
                    (move 0)
                    (move 1)
                    (move 2)
                    (move 3)
                    (to-vector))
                [1 2 -2 -3 0 3 4])
           (is= (-> (create-state [1 2 -3 3 -2 0 4])
                    (move 0)
                    (move 1)
                    (move 2)
                    (move 3)
                    (move 4)
                    (to-vector))
                [1 2 -3 0 3 4 -2])
           (is= (-> (create-state [1 2 -3 3 -2 0 4])
                    (move 0)
                    (move 1)
                    (move 2)
                    (move 3)
                    (move 4)
                    (move 5)
                    (to-vector))
                [1 2 -3 0 3 4 -2]))}
  ([state id]
   (move state id 1))
  ([state id factor]
   (let [element-from (get state id)
         value (* factor (:value element-from))
         rest-value (* (if (pos? value) 1 -1)
                       (mod (abs value) (dec (count state))))]
     (if (zero? rest-value)
       state
       (let [element-before-from (get state (:previous element-from))
             element-after-from (get state (:next element-from))
             element-at-to (reduce (fn [a _]
                                     (if (pos? rest-value)
                                       (get state (:next a))
                                       (get state (:previous a))))
                                   element-from
                                   (range (abs rest-value)))]
         (if (pos? rest-value)
           (let [element-after-to (get state (:next element-at-to))]
             (-> state
                 (assoc-in [(:id element-before-from) :next] (:id element-after-from)) ; ok
                 (assoc-in [(:id element-after-from) :previous] (:id element-before-from)) ; ok
                 (assoc-in [(:id element-at-to) :next] (:id element-from))
                 (assoc-in [(:id element-from) :previous] (:id element-at-to))
                 (assoc-in [(:id element-from) :next] (:id element-after-to))
                 (assoc-in [(:id element-after-to) :previous] (:id element-from))))
           (let [element-before-to (get state (:previous element-at-to))]
             (-> state
                 (assoc-in [(:id element-before-from) :next] (:id element-after-from))
                 (assoc-in [(:id element-after-from) :previous] (:id element-before-from))
                 (assoc-in [(:id element-at-to) :previous] (:id element-from))
                 (assoc-in [(:id element-from) :next] (:id element-at-to))
                 (assoc-in [(:id element-from) :previous] (:id element-before-to))
                 (assoc-in [(:id element-before-to) :next] (:id element-from))))))))))

(defn move-all
  [state]
  (reduce move state (range (count state))))

(defn move-all-with-factor
  [state factor]
  (reduce (fn [state id] (move state id factor)) state (range (count state))))


(defn get-puzzle-value
  [state]
  (let [first-element (->> state
                           (some (fn [{v :value :as element}] (when (zero? v) element))))
        element1000 (reduce (fn [a _]
                              (get state (:next a)))
                            first-element
                            (range 1000))
        element2000 (reduce (fn [a _]
                              (get state (:next a)))
                            element1000
                            (range 1000))
        element3000 (reduce (fn [a _]
                              (get state (:next a)))
                            element2000
                            (range 1000))]
    (+ (:value element1000)
       (:value element2000)
       (:value element3000))))

(comment
  (def mix (time (move-all state)))
  (def mix-b (time (reduce (fn [state times]
                             (move-all-with-factor state 811589153))
                           state
                           (range 10))))


  (def test-mix (move-all test-state))

  (get-puzzle-value test-mix)

  (->> mix
       (some (fn [{value :value :as e}] (when (zero? value) e))))

  (get-puzzle-value mix)
  (* (get-puzzle-value mix-b) 811589153)
  ; 1790365671518

  )




