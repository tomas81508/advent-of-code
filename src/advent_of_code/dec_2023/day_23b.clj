(ns advent-of-code.dec-2023.day-23b
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

(defn get-junctures
  [the-map]
  (reduce-kv (fn [junctures p _]
               (if (> (->> directions
                           (map (fn [d] (map + p d)))
                           (filter (fn [p] (contains? the-map p)))
                           (count))
                      2)
                 (conj junctures p)
                 junctures))
             #{}
             (dissoc the-map :size)))

(def test-junctures (get-junctures test-map))
(def prod-junctures (get-junctures prod-map))

(defn create-state
  [the-map]
  (let [goal [(- (:size the-map) 2) (dec (:size the-map))]
        junctures (get-junctures the-map)]
    (loop [queue (reduce (fn [queue juncture]
                           (conj queue
                                 {:position juncture :last-position nil :started-at juncture :steps 0}))
                         #{{:position [1 0] :last-position nil :started-at [1 0] :steps 0}}
                         junctures)
           result {}]
      (if (empty? queue)
        result
        (let [[w & rq] queue
              [queue result]
              (->> directions
                   (keep (fn [d]
                           (let [np (mapv + d (:position w))]
                             (when (and (contains? the-map np)
                                        (not= np (:last-position w)))
                               np))))
                   (reduce (fn [[queue result] np]
                             (let [n-steps (inc (:steps w))]
                               (cond (= np goal)
                                     [queue
                                      (update result (:started-at w)
                                              assoc np n-steps)]

                                     (contains? junctures np)
                                     [queue
                                      (update result (:started-at w)
                                              assoc np n-steps)]

                                     :else
                                     [(conj queue {:position      np
                                                   :last-position (:position w)
                                                   :started-at    (:started-at w)
                                                   :steps         n-steps})
                                      result])))
                           [rq result]))]
          (recur queue result))))))

(def test-state (create-state test-map))
(def prod-state (create-state prod-map))
(count prod-junctures)

(defn longest-unique-walk
  {:test (fn []
           (is= (longest-unique-walk test-map)
                154))}
  [the-map]
  (let [state (create-state the-map)
        goal [(- (:size the-map) 2) (dec (:size the-map))]]
    (loop [queue (list {:position [1 0] :steps 0 :visited #{[1 0]}})
           winner 0
           i 0]
      ;(println "Growing: " (->> queue
      ;                          (map :visited)
      ;                          (map count)
      ;                          (reduce min ##Inf)))
      ;(if (= i 3000000)
      ;  i)
      (if (empty? queue)
        winner
        (let [[w & rq] queue
              [queue winner]
              (reduce-kv (fn [[queue winner] np s]
                           (let [total-steps (+ (:steps w) s)]
                             (cond (contains? (:visited w) np)
                                   [queue winner]

                                   (= np goal)
                                   [queue (max winner total-steps)]

                                   :else
                                   (do
                                     ;(println np (count (:visited w)))
                                     ;  (println (:visited w))

                                     [(conj queue {:position np :steps total-steps
                                                   :visited  (conj (:visited w) np)})
                                      winner]))))
                         [rq winner]
                         (get state (:position w)))]
          (recur queue winner (inc i)))))))

(comment
  (time (longest-unique-walk prod-map))
  6710
  ; "Elapsed time: 56421.421438 msecs"
  )












