(ns advent-of-code.dec-2023.day-23
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]))

; Together with Daniel Gullberg

(def input (->> (slurp "src/advent_of_code/dec_2023/day_23_input.txt")
                (clojure.string/split-lines)
                (into [])))
(def test-input ["#.#####################"
                 "#.......#########...###"
                 "#######.#########.#.###"
                 "###.....#.>.>.###.#.###"
                 "###v#####.#v#.###.#.###"
                 "###.>...#.#.#.....#...#"
                 "###v###.#.#.#########.#"
                 "###...#.#.#.......#...#"
                 "#####.#.#.#######.#.###"
                 "#.....#.#.#.......#...#"
                 "#.#####.#.#.#########v#"
                 "#.#...#...#...###...>.#"
                 "#.#.#v#######v###.###v#"
                 "#...#.>.#...>.>.#.###.#"
                 "#####v#.#.###v#.#.###.#"
                 "#.....#...#...#.#.#...#"
                 "#.#########.###.#.#.###"
                 "#...###...#...#...#.###"
                 "###.###.#.###v#####v###"
                 "#...#...#.#.>.>.#.>.###"
                 "#.###.###.#.###.#.#v###"
                 "#.....###...###...#...#"
                 "#####################.#"])


(defn create-the-map
  [input]
  (-> (reduce-kv (fn [a y line]
                   (reduce-kv (fn [a x c]
                                (if (= c \#)
                                  a
                                  (assoc a [x y] c)))
                              a
                              (into [] line)))
                 {}
                 input)
      (assoc :size (count input))))

(def test-map (create-the-map test-input))
(def prod-map (create-the-map input))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn illegal-move?
  [d c]
  (contains? #{[[1 0] \<] [[-1 0] \>] [[0 1] \^] [[0 -1] \v]} [d c]))

(defn create-state
  [the-map]
  (let [goal [(- (:size the-map) 2) (dec (:size the-map))]]
    (loop [queue #{{:position [1 0] :last-position nil :started-at [1 0] :steps 0}}
           result {}]
      (if (empty? queue)
        result
        (let [[w & rq] queue
              [queue result]
              (->> directions
                   (keep (fn [d]
                           (let [np (mapv + d (:position w))]
                             (when (and (contains? the-map np)
                                        (not= np (:last-position w))
                                        (not (illegal-move? d (get the-map np))))
                               np))))
                   (reduce (fn [[queue result] np]
                             (let [n-steps (inc (:steps w))]
                               (cond (= np goal)
                                     [queue
                                      (update result (:started-at w)
                                              assoc np n-steps)]

                                     (= (get the-map np) \.)
                                     [(conj queue {:position      np
                                                   :last-position (:position w)
                                                   :started-at    (:started-at w)
                                                   :steps         n-steps})
                                      result]

                                     :else
                                     [(conj queue {:position      np
                                                   :last-position (:position w)
                                                   :started-at    np
                                                   :steps         0})
                                      (update result (:started-at w)
                                              assoc np n-steps)])))
                           [rq result]))]
          (recur queue result))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-map) 94))}
  [the-map]
  (let [state (create-state the-map)
        goal [(- (:size the-map) 2) (dec (:size the-map))]]
    (loop [paths (list {:position [1 0]
                        :steps    0})
           winner 0]
      (if (empty? paths)
        winner
        (let [[p & rp] paths
              [paths winner]
              (reduce-kv (fn [[paths winner] k v]
                           (let [new-steps (+ (:steps p) v)]
                             (if (= k goal)
                               [paths (max winner new-steps)]
                               [(conj paths {:position k :steps new-steps})
                                winner])))
                         [rp winner]
                         (get state (:position p)))]
          (recur paths winner)))))
  )

(comment
  (time (solve-a prod-map))
  )






















