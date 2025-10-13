(ns advent-of-code.dec-2018.day-15
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

(def input (->> (slurp "src/advent_of_code/dec_2018/day_15.txt")
                (string/split-lines)))

(def test-input-1 ["#######"
                   "#E..G.#"
                   "#...#.#"
                   "#.G.#G#"
                   "#######"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input-1)
                {:cavern   #{[2 2] [2 3] [3 3] [1 1] [5 3] [4 1] [5 2] [1 3] [5 1] [3 1] [2 1] [1 2] [3 2]},
                 :warriors #{{:position [1 1] :hp 200 :type :elf :id 0}
                             {:position [4 1] :hp 200 :type :goblin :id 1}
                             {:position [2 3] :hp 200 :type :goblin :id 2}
                             {:position [5 3] :hp 200 :type :goblin :id 3}}}))}
  [input]
  (->> input
       (reduce-kv (fn [state y row]
                    (->> (vec row)
                         (reduce-kv (fn [state x c]
                                      (cond-> state
                                              (#{\G \E} c) (update :warriors conj
                                                                   {:position [x y] :hp 200
                                                                    :type     (if (= c \G) :goblin :elf)
                                                                    :id       (count (:warriors state))})
                                              (#{\G \E \.} c) (update :cavern conj [x y])))
                                    state)))
                  {:cavern   #{}
                   :warriors #{}})))

(def state (create-state input))

(def test-state-1 (create-state test-input-1))

(def directions [[0 -1] [-1 0] [1 0] [0 1]])

(defn cavern?
  {:test (fn []
           (is (cavern? test-state-1 [2 2]))
           (is-not (cavern? test-state-1 [0 2])))}
  [state position]
  (contains? (:cavern state) position))

(defn get-warrior-by-position
  {:test (fn []
           (is= (get-warrior-by-position test-state-1 [1 1])
                {:position [1 1], :hp 200, :type :elf, :id 0})
           (is-not (get-warrior-by-position test-state-1 [10 10])))}
  [state position]
  (->> (:warriors state)
       (some (fn [{p :position :as e}] (when (= p position) e)))))

(defn get-warrior-by-id
  {:test (fn []
           (is= (get-warrior-by-id test-state-1 0)
                {:position [1 1], :hp 200, :type :elf, :id 0})
           (is-not (get-warrior-by-id test-state-1 10)))}
  [state id]
  (->> (:warriors state)
       (some (fn [w] (when (= id (:id w)) w)))))

(defn extend-path
  {:test (fn []
           (is= (extend-path test-state-1 (list [2 1] [1 1])
                             [0 1]
                             {:position [1 1], :hp 200, :type :elf, :id 0}
                             #{[1 2]})
                (list [2 2] [2 1] [1 1]))
           (is-not (extend-path (create-state ["GG."]) (list [0 0])
                                [1 0]
                                {:position [0 0], :hp 200, :type :goblin, :id 0}
                                #{})))}
  [state path direction warrior visited-path]
  (let [new-position (mapv + (first path) direction)]
    (when (and (cavern? state new-position)
               (not (contains? visited-path new-position))
               (not (contains? (into #{} path) new-position))
               (let [warrior-at-position (get-warrior-by-position state new-position)]
                 (not (= (:type warrior-at-position) (:type warrior)))))
      (conj path new-position))))

(defn filter-enemy-paths
  "Returns paths where an enemy is at the current position in the path."
  {:test (fn []
           (is= (filter-enemy-paths test-state-1
                                    [[[1 1]] [[2 3] [1 3]]]
                                    {:position [1 1], :hp 200, :type :elf, :id 0})
                [[[2 3] [1 3]]]))}
  [state paths warrior]
  (->> paths
       (filter (fn [[position]]
                 (when-let [other-warrior (get-warrior-by-position state position)]
                   (not= (:type other-warrior) (:type warrior)))))))


(defn basic-move
  {:test (fn []
           (is= (-> (basic-move test-state-1
                                {:position [1 1], :hp 200, :type :elf, :id 0}
                                [2 1])
                    (get-warrior-by-position [2 1])
                    (:id))
                0))}
  [state warrior to-position]
  (update state :warriors (fn [warriors]
                            (->> warriors
                                 (map (fn [w]
                                        (if (= w warrior)
                                          (assoc w :position to-position)
                                          w)))
                                 (set)))))

(defn move
  {:test (fn []
           (is (-> (move test-state-1 {:position [1 1], :hp 200, :type :elf, :id 0})
                   (get-warrior-by-position [2 1]))))}
  [state warrior]
  (loop [paths (vector (list (:position warrior)))]
    (let [visited-paths (->> paths
                             (reduce (fn [a path] (apply conj a path))
                                     #{}))
          extended-paths (->> paths
                              (mapcat (fn [path]
                                        (->> directions
                                             (keep (fn [d]
                                                     (extend-path state path d warrior visited-paths)))))))]
      (if (empty? extended-paths)
        state
        (let [enemy-paths (filter-enemy-paths state extended-paths warrior)]
          (if (empty? enemy-paths)
            (recur extended-paths)
            (let [first-path (first enemy-paths)]
              (if (= (count first-path) 2)
                state
                (let [next-position (first (take-last 2 first-path))]
                  (basic-move state warrior next-position))))))))))

(defn get-attack-targets
  {:test (fn []
           (is= (-> (create-state ["GE"])
                    (get-attack-targets {:position [0 0] :hp 200 :type :goblin :id 0}))
                [{:position [1 0] :hp 200 :type :elf :id 1}])
           (is= (-> (create-state ["G.E"])
                    (get-attack-targets {:position [0 0] :hp 200 :type :goblin :id 0}))
                []))}
  [state {position :position type :type}]
  (->> directions
       (map (fn [d] (map + position d)))
       (keep (fn [tp] (->> (get state :warriors)
                           (some (fn [{p :position t :type :as e}]
                                   (when (and (= p tp)
                                              (not= t type))
                                     e))))))))

(defn attack
  {:test (fn []
           (is= (-> (attack (create-state [".G." "GEG" ".G."])
                            {:position [1 1], :hp 200, :type :elf, :id 2}
                            [{:position [1 0], :hp 200, :type :goblin, :id 0}
                             {:position [1 2], :hp 200, :type :goblin, :id 4}
                             {:position [2 1], :hp 200, :type :goblin, :id 3}
                             {:position [0 1], :hp 200, :type :goblin, :id 1}])
                    (get-warrior-by-position [1 0])
                    (:hp))
                197)
           (is= (-> (attack {:cavern   #{[0 0] [1 0]},
                             :warriors #{{:position [0 0], :hp 200, :type :elf, :id 0}
                                         {:position [1 0], :hp 2, :type :goblin, :id 1}}}
                            {:position [0 0], :hp 200, :type :elf, :id 0}
                            [{:position [1 0], :hp 2, :type :goblin, :id 1}])
                    (:warriors)
                    (count))
                1))}
  [state warrior targets]
  (let [target (->> targets
                    (sort-by (juxt :hp (comp second :position) (comp first :position)))
                    (first))
        damaged-target (update target :hp - 3)]
    (update state :warriors (fn [warriors]
                              (let [other-warriors (disj warriors target)]
                                (if (pos? (:hp damaged-target))
                                  (conj other-warriors damaged-target)
                                  other-warriors))))))


(defn make-a-turn
  {:test (fn []
           (is= (-> (make-a-turn (create-state ["G..E"]))
                    (get-warrior-by-position [2 0])
                    (:hp))
                200)
           (is= (-> (make-a-turn (create-state ["G.E"]))
                    (get-warrior-by-position [2 0])
                    (:hp))
                197)
           (is= (-> (make-a-turn (create-state ["GE"]))
                    (get-warrior-by-position [1 0])
                    (:hp))
                197))}
  [state]
  (let [warriors (->> (:warriors state)
                      (sort-by (juxt (comp second :position)
                                     (comp first :position))))]
    (reduce (fn [state {id :id :as w}]
              (let [state (move state w)
                    w (get-warrior-by-id state id)
                    targets (get-attack-targets state w)]
                (if (empty? targets)
                  state
                  (attack state w targets))))
            state
            warriors)))

(defn state->string
  [state]
  (let [max-y (->> (:cavern state)
                   (map second)
                   (reduce max 0))
        max-x (->> (:cavern state)
                   (map first)
                   (reduce max 0))]
    (->> (for [y (range (+ 2 max-y))
               x (range (+ 2 max-x))]
           [x y])
         (partition (+ 2 max-x))
         (map (fn [row]
                (reduce (fn [a p]
                          (let [warrior (get-warrior-by-position state p)]
                            (str a
                                 (cond (= (:type warrior) :goblin) "G"
                                       (= (:type warrior) :elf) "E"
                                       (contains? (:cavern state) p) "."
                                       :else "#"))))
                        ""
                        row)))
         (string/join "\n"))))


(def test-case-1 (create-state ["#######"
                                "#.G...#"
                                "#...EG#"
                                "#.#.#G#"
                                "#..G#E#"
                                "#.....#"
                                "#######"]))

(println (state->string ((apply comp (repeat 37 make-a-turn))
                         (create-state ["#######"
                                        "#G..#E#"
                                        "#E#E.E#"
                                        "#G.##.#"
                                        "#...#E#"
                                        "#...E.#"
                                        "#######"]))))

(defn puzzle-1
  {:test (fn []
           (is= (puzzle-1 test-case-1)
                [47 590 27730])
           (is= (puzzle-1 (create-state ["#######"
                                         "#G..#E#"
                                         "#E#E.E#"
                                         "#G.##.#"
                                         "#...#E#"
                                         "#...E.#"
                                         "#######"]))
                [37 982 36334])
           (is= (puzzle-1 (create-state ["#######"
                                         "#E..EG#"
                                         "#.#G.E#"
                                         "#E.##E#"
                                         "#G..#.#"
                                         "#..E#.#"
                                         "#######"]))
                [46 859 39514])
           (is= (puzzle-1 (create-state ["#######"
                                         "#E.G#.#"
                                         "#.#G..#"
                                         "#G.#.G#"
                                         "#G..#.#"
                                         "#...E.#"
                                         "#######"]))
                [35 793 27755])
           (is= (puzzle-1 (create-state ["#######"
                                         "#.E...#"
                                         "#.#..G#"
                                         "#.###.#"
                                         "#E#G#G#"
                                         "#...#G#"
                                         "#######"]))
                [54 536 28944])
           (is= (puzzle-1 (create-state ["#########"
                                         "#G......#"
                                         "#.E.#...#"
                                         "#..##..G#"
                                         "#...##..#"
                                         "#...#...#"
                                         "#.G...G.#"
                                         "#.....G.#"
                                         "#########"]))
                [20 937 18740]))}
  [state]
  (loop [i 0
         state state]
    (let [next-state (make-a-turn state)]
      (if (= state next-state)
        (let [sum-of-hp (->> (:warriors state)
                             (map :hp)
                             (reduce +))]
          [i sum-of-hp (* i sum-of-hp)])
        (recur (inc i) next-state)))))

(comment
  (puzzle-1 state)
  )