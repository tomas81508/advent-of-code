(ns advent-of-code.dec-2024.day-10
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_10_input.txt")
                (clojure.string/split-lines)))

(def test-input-small ["0123"
                       "1234"
                       "8765"
                       "9876"])

(def test-input-large ["89010123"
                       "78121874"
                       "87430965"
                       "96549874"
                       "45678903"
                       "32019012"
                       "01329801"
                       "10456732"])

(defn create-atlas
  [input]
  (->> input
       (reduce-kv (fn [a y row]
                    (->> row
                         (into [])
                         (reduce-kv (fn [a x height]
                                      (assoc a [x y] (read-string (str height))))
                                    a)))
                  {})))

(def atlas (create-atlas input))
(def test-atlas-small (create-atlas test-input-small))
(def test-atlas-large (create-atlas test-input-large))

(defn get-start-positions
  {:test (fn []
           (is= (get-start-positions test-atlas-small)
                #{[0 0]}))}
  [atlas]
  (reduce-kv (fn [a k v] (if (= v 0) (conj a k) a))
             #{}
             atlas))

(defn create-state
  {:test (fn []
           (is= (create-state test-atlas-small)
                {:hikes {[0 0] #{[0 0]}}
                 :steps 0}))}
  [atlas]
  (let [start-positions (get-start-positions atlas)]
    {:hikes (reduce (fn [a sp] (assoc a sp #{sp}))
                    {}
                    start-positions)
     :steps 0}))

(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn next-positions-by-position
  {:test (fn []
           (is= (next-positions-by-position test-atlas-small [0 0] 1)
                #{[1 0] [0 1]}))}
  [atlas p height]
  (->> directions
       (map (fn [d] (map + p d)))
       (filter (fn [np] (= (get atlas np) height)))
       (into #{})))

(defn next-positions-by-positions
  {:test (fn []
           (is= (next-positions-by-positions test-atlas-small #{[1 0] [0 1]} 2)
                #{[1 1] [2 0]}))}
  [atlas ps height]
  (->> ps
       (map (fn [p] (next-positions-by-position atlas p height)))
       (apply clojure.set/union)))

(defn move-a-step
  {:test (fn []
           (is= (move-a-step test-atlas-small (create-state test-atlas-small))
                {:hikes {[0 0] #{[1 0] [0 1]}}
                 :steps 1}))}
  [atlas state]
  (-> state
      (update :steps inc)
      (update :hikes (fn [hikes]
                       (reduce-kv (fn [a start-position boundary]
                                    (assoc a start-position
                                             (next-positions-by-positions atlas
                                                                          boundary
                                                                          (inc (:steps state)))))
                                  {}
                                  hikes)))))

(defn walk
  {:test (fn []
           (is= (walk test-atlas-small) 1)
           (is= (walk test-atlas-large) 36))}
  [atlas]
  (loop [state (create-state atlas)]
    (let [state (move-a-step atlas state)]
      (if (= (:steps state) 9)
        (->> (:hikes state)
             (vals)
             (map count)
             (reduce +))
        (recur state)))))

(comment
  (walk test-atlas-small)
  (walk test-atlas-large)
  (time (walk atlas))
  )

; part two

(defn create-state2
  {:test (fn []
           (is= (create-state2 test-atlas-small)
                {:hikes {[0 0] 1}
                 :steps 0}))}
  [atlas]
  (let [start-positions (get-start-positions atlas)]
    {:hikes (reduce (fn [a sp] (assoc a sp 1))
                    {}
                    start-positions)
     :steps 0}))

(defn move-a-step2
  {:test (fn []
           (is= (move-a-step2 test-atlas-small (create-state2 test-atlas-small))
                {:hikes {[1 0] 1
                         [0 1] 1}
                 :steps 1})
           (is= (move-a-step2 test-atlas-small
                              (move-a-step2 test-atlas-small
                                            (create-state2 test-atlas-small)))
                {:hikes {[1 1] 2
                         [2 0] 1}
                 :steps 2}))}
  [atlas state]
  (-> state
      (update :steps inc)
      (update :hikes (fn [hikes]
                       (reduce-kv (fn [a position n]
                                    (let [next-positions (next-positions-by-position atlas
                                                                                     position
                                                                                     (inc (:steps state)))]
                                      (reduce (fn [a np]
                                                (update a np
                                                        (fn [m] (+ n (or m 0)))))
                                              a
                                              next-positions)))
                                  {}
                                  hikes)))))

(defn walk2
  {:test (fn []
           (is= (walk2 test-atlas-large) 81))}
  [atlas]
  (loop [state (create-state2 atlas)]
    (let [state (move-a-step2 atlas state)]
      (if (= (:steps state) 9)
        (->> (:hikes state)
             (vals)
             (reduce +))
        (recur state)))))

(comment
  (time (walk2 atlas))
  )



