(ns advent-of-code.dec-2021.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_09_input.txt"))

(def test-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")

(defn create-state
  [input]
  (->> input
       (clojure.string/split-lines)
       (mapv (fn [line]
               (mapv (fn [n]
                       (read-string (str n)))
                     line)))))

(def test-state (create-state test-input))
(def puzzle-state (create-state puzzle-input))

(defn find-x-mins
  {:test (fn []
           (is= (find-x-mins test-state)
                [[1 0] [9 0] [0 1] [3 1] [6 1] [9 1] [2 2] [7 2] [9 2] [2 3] [6 3] [1 4] [6 4]]))}
  [state]
  (->> state
       (map (fn [row]
              (->> (conj row 9)
                   (cons 9)
                   (partition 3 1)
                   (map-indexed (fn [x [n1 n2 n3]]
                                  [x (and (< n2 n1) (< n2 n3))]))
                   (filter (fn [[_ b]] b))
                   (map (fn [[x _]] x)))))
       (map-indexed (fn [y row]
                      (->> row
                           (map (fn [x] [x y])))))
       (reduce concat)
       (into [])))

(defn y-min?
  {:test (fn []
           (is (y-min? test-state [2 2]))
           (is-not (y-min? test-state [7 2])))}
  [state [x y]]
  (let [v (get-in state [y x])
        v-up (get-in state [(dec y) x] 9)
        v-down (get-in state [(inc y) x] 9)]
    (and (< v v-up) (< v v-down))))

(defn find-mins
  [state]
  (->> (find-x-mins state)
       (filter (fn [c] (y-min? state c)))))

(defn solver-a
  {:test (fn []
           (is= (solver-a test-state)
                15))}
  [state]
  (let [mins (->> (find-mins state)
                  (map (fn [[x y]]
                         (inc (get-in state [y x])))))]
    (reduce + mins)))

(comment
  (time (solver-a puzzle-state))
  ; "Elapsed time: 23.911843 msecs"
  )

(defn find-basin
  {:test (fn []
           (is= (find-basin test-state [1 0])
                #{[0 0] [1 0] [0 1]})
           (is= (count (find-basin test-state [9 0]))
                9))}
  [state low-point]
  (loop [border #{low-point}
         inner-points #{}]
    (let [neighbours (->> border
                          (map (fn [p]
                                 (->> [[1 0] [-1 0] [0 1] [0 -1]]
                                      (map (fn [d] (map + d p))))))
                          (reduce concat)
                          (set))
          new-border (->> (clojure.set/difference neighbours inner-points)
                          (filter (fn [[x y]] (< (get-in state [y x] 9) 9)))
                          (set))
          new-inner-points (clojure.set/union inner-points border)]
      (if (empty? new-border)
        new-inner-points
        (recur new-border new-inner-points)))))

(defn solver-b
  {:test (fn []
           (is= (solver-b test-state)
                1134))}
  [state]
  (->> (find-mins state)
       (map (fn [low-point] (count (find-basin state low-point))))
       (sort >)
       (take 3)
       (reduce *)))

(comment
  (time (solver-b puzzle-state))
  ; "Elapsed time: 67.37488 msecs"
  1123524
  )


