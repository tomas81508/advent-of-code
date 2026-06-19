(ns advent-of-code.dec-2021.day-23b
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.set :refer [union difference intersection]])
  (:import [clojure.lang PersistentQueue]))

(def puzzle-input ["#############"
                   "#...........#"
                   "###D#A#D#C###"
                   "  #D#C#B#A#"
                   "  #D#B#A#C#"
                   "  #B#C#B#A#"
                   "  #########"])

(def atlas {:spaces                #{[0 0] [1 0]
                                     [2 0] [2 1] [2 2] [2 3] [2 4]
                                     [3 0] [4 0] [4 1] [4 2] [4 3] [4 4]
                                     [5 0] [6 0] [6 1] [6 2] [6 3] [6 4]
                                     [7 0] [8 0] [8 1] [8 2] [8 3] [8 4]
                                     [9 0] [10 0]}
            :A-cave                #{[2 1] [2 2] [2 3] [2 4]}
            :B-cave                #{[4 1] [4 2] [4 3] [4 4]}
            :C-cave                #{[6 1] [6 2] [6 3] [6 4]}
            :D-cave                #{[8 1] [8 2] [8 3] [8 4]}
            :forbidden-stop-spaces #{[2 0] [4 0] [6 0] [8 0]}})

(def energy-costs {\A 1 \B 10 \C 100 \D 1000})

(defn create-state
  {:test (fn []
           (is= (create-state puzzle-input)
                {:A #{[8 4] [6 3] [4 1] [8 2]},
                 :B #{[4 3] [6 4] [2 4] [6 2]},
                 :C #{[8 3] [4 2] [8 1] [4 4]},
                 :D #{[2 2] [2 3] [6 1] [2 1]}}))}
  [input]
  (->> input
       (mapv vec)
       (reduce-kv (fn [a y line]
                    (->> line
                         (reduce-kv (fn [a x c]
                                      (if-not (contains? #{\A \B \C \D} c)
                                        a
                                        (update a (keyword (str c))
                                                conj [(dec x) (dec y)])))
                                    a)))
                  {:A #{} :B #{} :C #{} :D #{}})))

(def puzzle-state (create-state puzzle-input))

(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn get-amphipod-positions
  {:test (fn []
           (is= (get-amphipod-positions puzzle-state)
                #{[4 3] [2 2] [8 4] [2 3] [8 3] [6 3] [4 2] [4 1] [8 2] [6 4] [8 1] [6 1] [2 4] [2 1] [4 4] [6 2]}))}
  [state]
  (->> (vals state)
       (apply union)))

(defn get-amphipod
  [state position]
  (->> state
       (some (fn [[a ps]] (when (contains? ps position) a)))))

(defn get-my-home-raw
  {:test (fn []
           (is= (get-my-home-raw :C)
                #{[6 3] [6 4] [6 1] [6 2]}))}
  [amphipod]
  (get atlas (keyword (str (name amphipod) "-cave"))))
(def get-my-home (memoize get-my-home-raw))

(defn get-other-homes-raw
  {:test (fn []
           (is= (get-other-homes-raw :B)
                #{[2 2] [8 4] [2 3] [8 3] [6 3] [8 2] [6 4] [8 1] [6 1] [2 4] [2 1] [6 2]}))}
  [amphipod]
  (let [home-key (keyword (str (name amphipod) "-cave"))
        other-keys (disj #{:A-cave :B-cave :C-cave :D-cave} home-key)]
    (->> (select-keys atlas (seq other-keys))
         (vals)
         (apply union))))
(def get-other-homes (memoize get-other-homes-raw))

(defn all-at-home?
  {:test (fn []
           (is (all-at-home? {:A #{[2 1] [2 2] [2 3] [2 4]}} :A))
           (is-not (all-at-home? {:A #{[2 1] [4 2]}} :A)))}
  [state amphipod]
  (let [home (get-my-home amphipod)
        positions (get state amphipod)]
    (= home positions)))

(defn visiters-at-home?
  [state amphipod]
  (let [home (get-my-home amphipod)
        visiters (->> (select-keys state (disj #{:A :B :C :D} amphipod))
                      (vals)
                      (apply union))]
    (not (empty? (intersection home visiters)))))

(defn home-with-friends?
  {:test (fn []
           (is (home-with-friends? (create-state ["#############"
                                                  "#...B.......#"
                                                  "###A#.#C#D###"
                                                  "  #D#B#B#A#"
                                                  "  #D#B#A#C#"
                                                  "  #A#B#C#D#"
                                                  "  #########"])
                                   :B
                                   [4 2])))}
  [state amphipod position]
  (and (contains? (get-my-home amphipod) position)
       (not (visiters-at-home? state amphipod))))

(defn hallway? [position] (zero? (second position)))

(defn allowed-to-walk?
  {:test (fn []
           (is (allowed-to-walk? (create-state ["#############"
                                                "#...A...D...#"
                                                "###D#.#.#C###"
                                                "  #B#C#B#A#"
                                                "  #D#B#B#C#"
                                                "  #B#C#C#A#"
                                                "  #########"])
                                 #{}
                                 :C
                                 [4 1]
                                 [4 2])))}
  [state visited amphipod position starting-position]
  (and
    ; It should be a new position
    (not (contains? visited position))
    ; It should be on the map
    (contains? (:spaces atlas) position)
    ; It should be empty
    (not (contains? (get-amphipod-positions state) position))
    ; It can't be a home of another kind unless started there
    (or (hallway? position)
        (= (first starting-position) (first position))
        (contains? (get-my-home amphipod) position))))

(defn get-positions-below
  {:test (fn []
           (is= (get-positions-below [6 1])
                [[6 2] [6 3] [6 4]]))}
  [position]
  (loop [result []
         position (map + position [0 1])]
    (if (contains? (:spaces atlas) position)
      (recur (conj result position) (map + position [0 1]))
      result)))

(defn move-position?
  {:test (fn []
           (is (move-position? [5 0]
                               (create-state ["#############"
                                              "#...A...D...#"
                                              "###D#.#.#C###"
                                              "  #B#C#B#A#"
                                              "  #D#B#B#C#"
                                              "  #B#C#C#A#"
                                              "  #########"])
                               :C
                               [4 2]))
           (is-not (move-position? [6 1]
                                   (create-state ["#############"
                                                  "#...A...D...#"
                                                  "###D#.#.#C###"
                                                  "  #B#C#B#A#"
                                                  "  #D#B#B#C#"
                                                  "  #B#C#C#A#"
                                                  "  #########"])
                                   :C
                                   [4 2])))}
  [position state amphipod starting-position]
  (and
    ; Not outside a home
    (not (contains? (:forbidden-stop-spaces atlas) position))
    ; Not a hallway -> hallway move
    (not (and (hallway? starting-position) (hallway? position)))
    ; Not another's home
    (not (contains? (get-other-homes amphipod) position))
    ; If inside our own home all below us must be our kind
    (or (not (contains? (get-my-home amphipod) position))
        (->> (get-positions-below position)
             (every? (fn [p] (contains? (get state amphipod) p)))))))

(defn get-move-positions
  {:test (fn []
           (is= (get-move-positions puzzle-state [4 1])
                #{[0 0] [1 0] [3 0] [5 0] [7 0] [9 0] [10 0]})
           (is= (get-move-positions (create-state ["#############"
                                                   "#...A...D...#"
                                                   "###D#.#.#C###"
                                                   "  #B#C#B#A#"
                                                   "  #D#B#B#C#"
                                                   "  #B#C#C#A#"
                                                   "  #########"])
                                    [4 2])
                #{[5 0]})
           (is= (get-move-positions (create-state ["#############"
                                                   "#...A...D...#"
                                                   "###D#.#.#B###"
                                                   "  #D#C#.#A#"
                                                   "  #D#B#.#C#"
                                                   "  #B#C#C#A#"
                                                   "  #########"])
                                    [4 2])
                #{[6 3]})
           (is= (get-move-positions (create-state ["#############"
                                                   "#...A...D.C.#"
                                                   "###D#.#.#B###"
                                                   "  #D#C#.#A#"
                                                   "  #D#B#.#C#"
                                                   "  #B#C#.#A#"
                                                   "  #########"])
                                    [4 2])
                #{[6 4]})
           (is= (get-move-positions (create-state ["#############"
                                                   "#...........#"
                                                   "###.#.#.#.###"
                                                   "  #.#.#.#.#"
                                                   "  #A#B#.#.#"
                                                   "  #A#C#.#.#"
                                                   "  #########"])
                                    [4 3])
                #{[0 0] [1 0] [3 0] [5 0] [7 0] [9 0] [10 0]})
           (is= (get-move-positions (create-state ["#############"
                                                   "#...A...D...#"
                                                   "###.#D#.#C###"
                                                   "  #D#C#B#A#"
                                                   "  #D#B#A#C#"
                                                   "  #B#C#B#A#"
                                                   "  #########"])
                                    [3 0])
                #{})
           (is= (get-move-positions (create-state ["#############"
                                                   "#...B.......#"
                                                   "###A#.#C#D###"
                                                   "  #D#B#B#A#"
                                                   "  #D#B#A#C#"
                                                   "  #A#B#C#D#"
                                                   "  #########"])
                                    [3 0])
                #{[4 1]})
           (is= (get-move-positions (create-state ["#############"
                                                   "#...B.......#"
                                                   "###A#.#C#D###"
                                                   "  #A#B#C#D#"
                                                   "  #A#B#C#D#"
                                                   "  #A#B#C#D#"
                                                   "  #########"])
                                    [4 2])
                #{})
           (is= (get-move-positions puzzle-state [4 2])
                #{})
           (is= (get-move-positions puzzle-state [1 0])
                #{}))}
  [state position]
  (let [amphipod-positions (get-amphipod-positions state)
        amphipod (get-amphipod state position)
        started-inside-a-home (pos? (second position))]
    (if (or (not amphipod)
            (all-at-home? state amphipod)
            (home-with-friends? state amphipod position))
      #{}
      (loop [move-positions #{}
             currents (conj PersistentQueue/EMPTY position)
             visited #{}]
        (let [cp (peek currents)
              currents (pop currents)]
          (if-not cp
            ; If home is in the allowed moves ignore the other moves
            (if-let [home-move-position (->> move-positions
                                             (some (fn [mp] (when (pos? (second mp)) mp))))]
              #{home-move-position}
              move-positions)
            (let [new-positions (->> directions
                                     (map (fn [d] (map + d cp)))
                                     (filter (fn [p] (allowed-to-walk? state visited amphipod p position)))
                                     (into #{}))
                  new-move-positions (->> new-positions
                                          (filter (fn [p] (move-position? p state amphipod position))))]
              (recur (apply conj move-positions new-move-positions)
                     (apply conj currents new-positions)
                     (apply conj visited new-positions)))))))))

(defn distance
  {:test (fn []
           (is= (distance [3 2] [5 2]) 6))}
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs y1)
     (abs y2)))

(def energy-cost {:A 1 :B 10 :C 100 :D 1000})

(defn walk-a-step
  {:test (fn []
           (is= (count (walk-a-step {:state puzzle-state :energy 10})) 28)
           (is= (walk-a-step {:state  (create-state ["#############"
                                                     "#...B.......#"
                                                     "###A#.#C#D###"
                                                     "  #A#B#C#D#"
                                                     "  #A#B#C#D#"
                                                     "  #A#B#C#D#"
                                                     "  #########"])
                              :energy 10})
                [{:state  {:A #{[2 2] [2 3] [2 4] [2 1]},
                           :B #{[4 3] [4 2] [4 1] [4 4]},
                           :C #{[6 3] [6 4] [6 1] [6 2]},
                           :D #{[8 4] [8 3] [8 2] [8 1]}},
                  :energy 30}]))}
  [configuration]
  (->> (vals (:state configuration))
       (apply union)
       (mapcat (fn [p]
                 (let [state (:state configuration)
                       move-positions (get-move-positions state p)]
                   (->> move-positions
                        (map (fn [to-p]
                               (let [amphipod (get-amphipod state p)
                                     energy-cost (* (distance p to-p)
                                                    (energy-cost amphipod))]
                                 (-> configuration
                                     (update-in [:state amphipod] disj p)
                                     (update-in [:state amphipod] conj to-p)
                                     (update :energy + energy-cost)))))))))))

(def done-state {:A #{[2 1] [2 2] [2 3] [2 4]}
                 :B #{[4 1] [4 2] [4 3] [4 4]}
                 :C #{[6 1] [6 2] [6 3] [6 4]}
                 :D #{[8 1] [8 2] [8 3] [8 4]}})

(defn cmp-config [a b]
  (let [ea (:energy a)
        eb (:energy b)]
    (if (not= ea eb)
      (compare ea eb)
      ;; tie-break so different states with same energy are not treated equal
      (compare (pr-str (:state a)) (pr-str (:state b))))))

(defn walk
  {:test (fn []
           (is= (walk (create-state ["#############"
                                     "#...B.......#"
                                     "###A#.#C#D###"
                                     "  #A#B#C#D#"
                                     "  #A#B#C#D#"
                                     "  #A#B#C#D#"
                                     "  #########"]))
                #{{:state  {:A #{[2 2] [2 3] [2 4] [2 1]},
                            :B #{[4 3] [4 2] [4 1] [4 4]},
                            :C #{[6 3] [6 4] [6 1] [6 2]},
                            :D #{[8 4] [8 3] [8 2] [8 1]}},
                   :energy 20}}))}
  [state]
  (loop [configurations (sorted-set-by cmp-config {:state state :energy 0})
         result #{}
         min-energy 100000000]
    (if (or (empty? configurations)
            (-> (first configurations)
                (:energy)
                (> min-energy)))
      result
      (let [min-energy-configuration (first configurations)
            rest-of-configurations (disj configurations min-energy-configuration)
            new-configurations (walk-a-step min-energy-configuration)
            grouped (group-by (fn [{state :state}] (= state done-state)) new-configurations)
            done-configurations (get grouped true [])
            intermediate-configurations (get grouped false [])]
        (recur (apply conj rest-of-configurations intermediate-configurations)
               (apply conj result done-configurations)
               (if (empty? done-configurations)
                 min-energy
                 (min min-energy
                      (->> done-configurations
                           (map :energy)
                           (apply min)))))))))

(comment
  (time (->> (walk puzzle-state)
             (map :energy)
             (sort)
             (first)))
  ; "Elapsed time: 71132.728 msecs"
  ; => 40954
  )


