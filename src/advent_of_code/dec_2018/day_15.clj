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
                 :warriors {[1 1] {:hp 200 :type :elf :id 0}
                            [4 1] {:hp 200 :type :goblin :id 1}
                            [2 3] {:hp 200 :type :goblin :id 2}
                            [5 3] {:hp 200 :type :goblin :id 3}}}))}
  [input]
  (->> input
       (reduce-kv (fn [state y row]
                    (->> (vec row)
                         (reduce-kv (fn [state x c]
                                      (cond-> state
                                              (#{\G \E} c) (update :warriors assoc
                                                                   [x y]
                                                                   {:hp   200
                                                                    :type (if (= c \G) :goblin :elf)
                                                                    :id   (count (:warriors state))})
                                              (#{\G \E \.} c) (update :cavern conj [x y])))
                                    state)))
                  {:cavern   #{}
                   :warriors {}})))

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
                {:hp 200 :type :elf :id 0})
           (is-not (get-warrior-by-position test-state-1 [10 10])))}
  [state position]
  (get (:warriors state) position))

(defn get-warrior-by-id
  {:test (fn []
           (is= (get-warrior-by-id test-state-1 0)
                [[1 1] {:hp 200 :type :elf :id 0}])
           (is-not (get-warrior-by-id test-state-1 10)))}
  [state id]
  (->> (:warriors state)
       (some (fn [[p w]] (when (= id (:id w)) [p w])))))

(defn filter-enemy-paths
  "Returns paths where an enemy is at the current position in the path."
  {:test (fn []
           (is= (filter-enemy-paths test-state-1
                                    [[[2 1] [4 1]] [[1 2] [1 4]]]
                                    {:hp 200 :type :elf :id 0})
                [[[2 1] [4 1]]]))}
  [state paths warrior]
  (->> paths
       (filter (fn [[_ position]]
                 (when-let [other-warrior (get-warrior-by-position state position)]
                   (not= (:type other-warrior) (:type warrior)))))))


(defn basic-move
  {:test (fn []
           (is= (-> (basic-move test-state-1
                                {:hp 200, :type :elf, :id 0}
                                [1 1]
                                [2 1])
                    (get-warrior-by-position [2 1])
                    (:id))
                0))}
  [state warrior from-position to-position]
  (update state :warriors (fn [warriors]
                            (-> warriors
                                (dissoc from-position)
                                (assoc to-position warrior)))))

(defn free-space?
  {:test (fn []
           (is (free-space? (create-state ["E.G"]) [1 0]))
           (is-not (free-space? (create-state ["E.G"]) [0 0])))}
  [state p]
  (and (cavern? state p)
       (not (get-warrior-by-position state p))))

(defn get-positions-in-range
  {:test (fn []
           (is= (get-positions-in-range test-state-1 {:hp 200 :type :goblin :id 1})
                #{[1 2] [2 1]}))}
  [state warrior]
  (->> (:warriors state)
       (filter (fn [[_ {t :type}]] (not= t (:type warrior))))
       (mapcat (fn [[p _]] (->> directions
                                (map (fn [d] (map + p d))))))
       (filter (fn [p] (free-space? state p)))
       (set)))

(defn enemy?
  [state warrior position]
  (when-let [w (get-warrior-by-position state position)]
    (not= (:type w) (:type warrior))))

(defn next-to-enemy?
  [state position warrior]
  (->> directions
       (map (fn [d] (map + position d)))
       (some (fn [p] (enemy? state warrior p)))))

(defn move
  {:test (fn []
           (is (-> (move test-state-1 [1 1] {:hp 200, :type :elf, :id 0})
                   (get-warrior-by-position [2 1]))))}
  [state position warrior]
  (if (next-to-enemy? state position warrior)
    state
    (let [positions-in-range (get-positions-in-range state warrior)]
      (if (empty? positions-in-range)
        state
        (loop [current-positions [[nil position]]           ; [first-step actual-step]
               visited #{position}]
          (let [extended-paths (->> current-positions
                                    (mapcat (fn [[first-step cp :as path]]
                                              (->> directions
                                                   (keep (fn [d]
                                                           (let [new-position (map + cp d)]
                                                             (when (and (cavern? state new-position)
                                                                        (not (contains? visited new-position))
                                                                        (let [warrior-at-position (get-warrior-by-position state new-position)]
                                                                          (not (= (:type warrior-at-position) (:type warrior)))))
                                                               (if (nil? first-step)
                                                                 [new-position new-position]
                                                                 (assoc path 1 new-position)))))))))
                                    (reduce (fn [{visited :visited :as a} [sp cp :as path]]
                                              (if (contains? visited cp)
                                                a
                                                (-> a
                                                    (update :paths conj path)
                                                    (update :visited conj cp))))
                                            {:paths   []
                                             :visited #{}})
                                    (:paths))]
            (if (empty? extended-paths)
              state
              (let [enemy-paths (->> extended-paths
                                     (filter (fn [[_ p]] (contains? positions-in-range p))))]
                (if (empty? enemy-paths)
                  (recur extended-paths (apply conj visited (map second extended-paths)))
                  (let [next-position (->> enemy-paths
                                           (sort-by (juxt (comp second second) (comp first second)))
                                           (ffirst))]
                    (basic-move state warrior position next-position)))))))))))

(defn get-attack-targets
  {:test (fn []
           (is= (-> (create-state ["GE"])
                    (get-attack-targets [0 0] {:hp 200 :type :goblin :id 0}))
                [{:position [1 0] :warrior {:hp 200 :type :elf :id 1}}])
           (is= (-> (create-state ["G.E"])
                    (get-attack-targets [0 0] {:hp 200 :type :goblin :id 0}))
                []))}
  [state position {type :type}]
  (->> directions
       (map (fn [d] (map + position d)))
       (keep (fn [tp] (when-let [target (get-warrior-by-position state tp)]
                        (when (not= (:type target) type)
                          {:position tp :warrior target}))))))

(def targets-sorter (juxt (comp :hp :warrior) (comp second :position) (comp first :position)))

(defn attack
  {:test (fn []
           (is= (-> (attack (create-state [".G." "GEG" ".G."])
                            [1 1]
                            {:hp 200, :type :elf, :id 2})
                    (get-warrior-by-position [1 0])
                    (:hp))
                197)
           (is= (-> (attack (create-state ["..." "GEG" ".G."])
                            [1 1]
                            {:hp 200, :type :elf, :id 2})
                    (get-warrior-by-position [0 1])
                    (:hp))
                197)
           (is= (-> (create-state ["..." "GEG" ".G."])
                    (assoc-in [:warriors [1 2] :hp] 100)
                    (attack [1 1] {:hp 200, :type :elf, :id 2})
                    (get-warrior-by-position [1 2])
                    (:hp))
                97)
           (is= (-> (attack {:cavern   #{[0 0] [1 0]},
                             :warriors {[0 0] {:hp 200 :type :elf :id 0}
                                        [1 0] {:hp 2 :type :goblin :id 1}}}
                            [0 0]
                            {:hp 200, :type :elf, :id 0})
                    (:warriors)
                    (count))
                1))}
  [state position warrior]
  (let [targets (get-attack-targets state position warrior)]
    (if (empty? targets)
      state
      (let [{tp :position tw :warrior} (->> targets
                                            (sort-by targets-sorter)
                                            (first))
            damaged-warrior (update tw :hp - 3)]
        (if (pos? (:hp damaged-warrior))
          (assoc-in state [:warriors tp] damaged-warrior)
          (update state :warriors dissoc tp))))))

(def warrior-types #{:elf :goblin})

(defn end-game?
  {:test (fn []
           (is (end-game? (create-state ["GG.."])))
           (is-not (end-game? (create-state ["G..E"])))
           (is-not (end-game? (create-state ["GG..EE"]))))}
  [state]
  (let [w-types (->> (:warriors state)
                     (vals)
                     (reduce (fn [a w]
                               (cond (= a warrior-types) (reduced false)
                                     (contains? a (:type w)) a
                                     :else (conj a (:type w))))
                             #{}))]
    (and w-types (not= w-types warrior-types))))

(def warrior-sorter (juxt (comp second) (comp first)))

(def test-case-1 (create-state ["#######"
                                "#.G...#"
                                "#...EG#"
                                "#.#.#G#"
                                "#..G#E#"
                                "#.....#"
                                "#######"]))

(defn get-sorted-warriors
  {:test (fn []
           (is= (get-sorted-warriors test-state-1)
                [[1 1] [4 1] [2 3] [5 3]]))}
  [state]
  (->> (:warriors state)
       (keys)
       (sort-by warrior-sorter)))

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
  (let [warrior-positions (->> (:warriors state)
                               (keys)
                               (sort-by warrior-sorter))]
    (reduce (fn [state p]
              (if (end-game? state)
                (reduced (assoc state :end true))
                (let [w (get-warrior-by-position state p)
                      move-state (move state p w)
                      [p w] (if (= move-state state)
                              [p w]
                              (get-warrior-by-id move-state (:id w)))]
                  (attack move-state p w))))
            state
            warrior-positions)))

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
                [20 937 18740])
           )}
  [state]
  (loop [rounds 0
         state state]
    (let [next-state (make-a-turn state)]
      (println rounds (end-game? next-state))
      (if (:end next-state)
        (let [sum-of-hp (->> (:warriors next-state)
                             (vals)
                             (map :hp)
                             (reduce +))]
          [rounds sum-of-hp (* rounds sum-of-hp)])
        (recur (inc rounds) next-state)))))

(comment
  (puzzle-1 state)
  ; 210737 too high [83 2539 210737]
  ; 206558 too low [82 2519 206558]
  )
