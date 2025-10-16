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
  (get-in state [:warriors position]))

(defn get-warrior-by-id
  {:test (fn []
           (is= (get-warrior-by-id test-state-1 0)
                [[1 1] {:hp 200 :type :elf :id 0}])
           (is-not (get-warrior-by-id test-state-1 10)))}
  [state id]
  (->> (:warriors state)
       (some (fn [[p w]] (when (= id (:id w)) [p w])))))


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

(defn enemies?
  [{t1 :type} {t2 :type}]
  {:pre [t1 t2]}
  (not= t1 t2))

(defn get-positions-in-range
  {:test (fn []
           (is= (get-positions-in-range test-state-1 {:hp 200 :type :goblin :id 1})
                #{[1 2] [2 1]}))}
  [state warrior]
  (->> (:warriors state)
       (filter (fn [[_ t]] (enemies? t warrior)))
       (mapcat (fn [[p _]] (->> directions (map (fn [d] (map + p d))))))
       (filter (fn [p] (free-space? state p)))
       (set)))

(defn enemy?
  [state warrior position]
  (when-let [w (get-warrior-by-position state position)]
    (enemies? w warrior)))

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
                                                                          (or (nil? warrior-at-position)
                                                                              (enemies? warrior-at-position warrior))))
                                                               (if first-step
                                                                 (assoc path 1 new-position)
                                                                 [new-position new-position]))))))))
                                    (reduce (fn [{visited :visited :as a} [_ cp :as path]]
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
  [state position warrior]
  (->> directions
       (map (fn [d] (map + position d)))
       (keep (fn [tp] (when-let [target (get-warrior-by-position state tp)]
                        (when (enemies? target warrior)
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
            damaged-warrior (update tw :hp - (get warrior :attack 3))]
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
  (let [warriors (vals (:warriors state))
        first-warrior (first warriors)]
    (->> warriors
         (some (fn [w] (enemies? w first-warrior)))
         (not))))

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
           (is= (-> (make-a-turn {:cavern   #{[0 0] [1 0]},
                                  :warriors {[0 0] {:hp 2, :type :goblin, :id 0},
                                             [1 0] {:hp 2, :type :elf, :id 1}}})
                    (get-warrior-by-position [0 0])
                    (:hp))
                2)
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
                (if-let [w (get-warrior-by-position state p)]
                  (let [move-state (move state p w)
                        [p w] (if (= move-state state)
                                [p w]
                                (get-warrior-by-id move-state (:id w)))]
                    (attack move-state p w))
                  state)))
            state
            warrior-positions)))

(defn elf?
  [warrior]
  (= (:type warrior) :elf))

(defn goblin?
  [warrior]
  (= (:type warrior) :goblin))

(defn state->string
  [state]
  (let [max-y (->> (:cavern state)
                   (map second)
                   (reduce max 0))
        max-x (->> (:cavern state)
                   (map first)
                   (reduce max 0))]
    (->> (for [y (range 1 (+ 1 max-y))
               x (range 1 (+ 1 max-x))]
           [x y])
         (partition max-x)
         (map (fn [row]
                (let [stuff (->> row
                                 (map (fn [p]
                                        (if-let [warrior (get-warrior-by-position state p)]
                                          [(if (goblin? warrior) "G" "E") (:hp warrior)]
                                          [(if (contains? (:cavern state) p) "." "#") nil]))))]
                  (str (apply str (map first stuff))
                       " "
                       (str "(" (clojure.string/join ") (" (keep second stuff)) ")\n")))))
         (apply str))))

(defn sum-hp
  [state]
  (->> (:warriors state)
       (vals)
       (map :hp)
       (reduce + 0)))

(defn fight
  {:test (fn []
           (is= (fight test-case-1)
                [47 590 27730])
           (is= (fight (create-state ["#######"
                                      "#G..#E#"
                                      "#E#E.E#"
                                      "#G.##.#"
                                      "#...#E#"
                                      "#...E.#"
                                      "#######"]))
                [37 982 36334])
           (is= (fight (create-state ["#######"
                                      "#E..EG#"
                                      "#.#G.E#"
                                      "#E.##E#"
                                      "#G..#.#"
                                      "#..E#.#"
                                      "#######"]))
                [46 859 39514])
           (is= (fight (create-state ["#######"
                                      "#E.G#.#"
                                      "#.#G..#"
                                      "#G.#.G#"
                                      "#G..#.#"
                                      "#...E.#"
                                      "#######"]))
                [35 793 27755])
           (is= (fight (create-state ["#######"
                                      "#.E...#"
                                      "#.#..G#"
                                      "#.###.#"
                                      "#E#G#G#"
                                      "#...#G#"
                                      "#######"]))
                [54 536 28944])
           (is= (fight (create-state ["#########"
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
  ([state]
   (fight state 3))
  ([state elf-attack]
   (let [state (update state :warriors
                       (fn [warriors]
                         (->> warriors
                              (reduce-kv (fn [warriors k w]
                                           (if (goblin? w)
                                             (assoc-in warriors [k :attack] 3)
                                             (assoc-in warriors [k :attack] elf-attack)))
                                         warriors))))]
     (loop [rounds 0
            state state]
       (let [next-state (make-a-turn state)]
         (if (:end next-state)
           (let [sum-of-hp (sum-hp next-state)]
             (println (state->string next-state))
             [rounds sum-of-hp (* rounds sum-of-hp)])
           (recur (inc rounds) next-state)))))))

(comment
  (time (fight state 3))
  ; "Elapsed time: 238.411583 msecs"
  ; => [82 2531 207542]
  )

(defn calculate-number-of-elves
  [state]
  (->> (:warriors state)
       (vals)
       (filter elf?)
       (count)))

(defn fight-without-loosing-an-elf
  {:test (fn []
           (is= (fight-without-loosing-an-elf test-case-1)
                [29 172 4988])
           (is= (fight-without-loosing-an-elf (create-state ["#######"
                                                             "#E..EG#"
                                                             "#.#G.E#"
                                                             "#E.##E#"
                                                             "#G..#.#"
                                                             "#..E#.#"
                                                             "#######"]))
                [33 948 31284])
           (is= (fight-without-loosing-an-elf (create-state ["#######"
                                                             "#E.G#.#"
                                                             "#.#G..#"
                                                             "#G.#.G#"
                                                             "#G..#.#"
                                                             "#...E.#"
                                                             "#######"]))
                [37 94 3478])
           (is= (fight-without-loosing-an-elf (create-state ["#######"
                                                             "#.E...#"
                                                             "#.#..G#"
                                                             "#.###.#"
                                                             "#E#G#G#"
                                                             "#...#G#"
                                                             "#######"]))
                [39 166 6474])
           (is= (fight-without-loosing-an-elf (create-state ["#########"
                                                             "#G......#"
                                                             "#.E.#...#"
                                                             "#..##..G#"
                                                             "#...##..#"
                                                             "#...#...#"
                                                             "#.G...G.#"
                                                             "#.....G.#"
                                                             "#########"]))
                [30 38 1140])
           )}
  [state]
  (let [number-of-elves (calculate-number-of-elves state)]
    (loop [elf-attack 4]
      (println elf-attack)
      (let [state (update state :warriors
                          (fn [warriors]
                            (->> warriors
                                 (reduce-kv (fn [warriors k w]
                                              (if (goblin? w)
                                                (assoc-in warriors [k :attack] 3)
                                                (assoc-in warriors [k :attack] elf-attack)))
                                            warriors))))
            [rounds result-state] (loop [rounds 0
                                         state state]
                                    (let [state (make-a-turn state)]
                                      (if (:end state)
                                        [rounds state]
                                        (recur (inc rounds) state))))]
        (if (= (calculate-number-of-elves result-state) number-of-elves)
          (let [sum-of-hp (sum-hp result-state)]
            (println (state->string result-state))
            [rounds sum-of-hp (* rounds sum-of-hp)])
            (recur (inc elf-attack)))))))

(comment
  (time (fight-without-loosing-an-elf state))
  ; "Elapsed time: 2308.324958 msecs"
  ; => [52 1244 64688]

  )