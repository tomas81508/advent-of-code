(ns advent-of-code.dec-2022.day-18
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def input (slurp "src/advent_of_code/dec_2022/day_18_input.txt"))
(def test-input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5")

(defn input->points
  [input]
  (->> input
       (clojure.string/split-lines)
       (map (fn [p] (->> (clojure.string/split p #",")
                         (mapv read-string))))))

(def test-points (input->points test-input))
(def points (input->points input))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [1 3 -2] [4 4 4])
                (+ 3 1 6)))}
  [p1 p2]
  (->> (map - p1 p2)
       (map (fn [c] (if (pos? c) c (- c))))
       (reduce +)))

(defn points->state
  [points]
  (let [origin [0 0 0]]
    (->> points
         (reduce (fn [a p] (assoc a p (manhattan-distance p origin)))
                 {}))))

(def test-state (points->state test-points))
(def state (points->state points))

(defn state->distance-state
  [state]
  (reduce-kv (fn [a k v] (update a v (fn [ks] (conj (or ks #{}) k))))
             {}
             state))

(def distance-test-state (state->distance-state test-state))

(defn get-potential-neighbors
  [distance distance-state]
  (concat (get distance-state (dec distance))
          (get distance-state distance)
          (get distance-state (inc distance))))

(defn solve-a
  [state]
  (let [distance-state (state->distance-state state)
        points (keys state)
        _ (println distance-state)
        neighbors-count (loop [[p & points] points
                               distance-state distance-state
                               neighbors 0]
                          (if (empty? points)
                            neighbors
                            (let [p-distance (get state p)
                                  neighbors-of-p (->> (get-potential-neighbors p-distance distance-state)
                                                      (filter (fn [op] (= (manhattan-distance p op) 1)))
                                                      (count)
                                                      (* 2))]
                              (recur points
                                     (update distance-state p-distance disj p)
                                     (+ neighbors neighbors-of-p)))))]
    (- (* 6 (count points))
       neighbors-count)))

(comment
  (solve-a {[1 1 2] 4 [1 1 1] 3})
  (solve-a test-state)
  (time (solve-a state))
  ; "Elapsed time: 233.324485 msecs"
  ; => 3374
  )



