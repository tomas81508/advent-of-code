(ns advent-of-code.dec-2018.day-24
  (:require [advent-of-code.test :refer [is=]]))

(def test-state-1
  {:immune    {:group1 {:id         [:immune :group1]
                        :units      17
                        :hit-points 5390
                        :weakness   #{:radiation :bludgeoning}
                        :attack     4507
                        :type       :fire
                        :initiative 2}
               :group2 {:id         [:immune :group2]
                        :units      989
                        :hit-points 1274
                        :immune     #{:fire}
                        :weakness   #{:bludgeoning :slashing}
                        :attack     25
                        :type       :slashing
                        :initiative 3}}
   :infection {:group1 {:id         [:infection :group1]
                        :units      801
                        :hit-points 4706
                        :weakness   #{:radiation}
                        :attack     116
                        :type       :bludgeoning
                        :initiative 1}
               :group2 {:id         [:infection :group2]
                        :units      4485
                        :hit-points 2961
                        :immune     #{:radiation}
                        :weakness   #{:fire :cold}
                        :attack     12
                        :type       :slashing
                        :initiative 4}}})

(defn calculate-damage
  {:test (fn []
           (is= (calculate-damage test-state-1 [:infection :group1] [:immune :group1]) 185832)
           (is= (calculate-damage test-state-1 [:infection :group1] [:immune :group2]) 185832)
           (is= (calculate-damage test-state-1 [:infection :group2] [:immune :group2]) 107640)
           (is= (calculate-damage test-state-1 [:immune :group1] [:infection :group1]) 76619)
           (is= (calculate-damage test-state-1 [:immune :group1] [:infection :group2]) 153238)
           (is= (calculate-damage test-state-1 [:immune :group2] [:infection :group1]) 24725)
           (is= (calculate-damage test-state-1 [:immune :group2] [:infection :group2]) 24725))}
  [state attacker-id defender-id]
  (let [attacker (get-in state attacker-id)
        defender (get-in state defender-id)]
    (if (contains? (:immune defender) (:type attacker))
      0
      (* (:units attacker)
         (:attack attacker)
         (if (contains? (:weakness defender) (:type attacker)) 2 1)))))

(defn get-killed-units
  {:test (fn []
           (is= (get-killed-units test-state-1 [:infection :group2] [:immune :group2]) 84)
           (is= (get-killed-units test-state-1 [:immune :group1] [:infection :group2]) 51)
           (is= (get-killed-units test-state-1 [:infection :group1] [:immune :group1]) 17))}
  [state attacker-id defender-id]
  (let [defender (get-in state defender-id)
        damage (calculate-damage state attacker-id defender-id)
        killed-units (min (quot damage (:hit-points defender))
                          (:units defender))]
    (println "A" attacker-id "D" defender-id "damage" damage "ku" killed-units)
    killed-units))

(defn get-group-ids
  [state]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (map :id)
       (set)))


(defn get-group
  [state id]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (some (fn [g] (when (= (:id g) id) g)))))

(defn get-effective-power
  [state id]
  (let [group (get-group state id)]
    (* (:units group) (:attack group))))

(defn get-target-selection-order
  {:test (fn []
           (is= (get-target-selection-order test-state-1)
                [[:infection :group1] [:immune :group1] [:infection :group2] [:immune :group2]]))}
  [state]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (map (fn [g] (assoc g :effective-power (* (:units g) (:attack g)))))
       (sort-by (juxt :effective-power :initiative) #(compare %2 %1))
       (map :id)))

(defn get-initiative-rank
  [state]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (sort-by :initiative)
       (reverse)
       (map :id)))

(defn same-team?
  [group1 group2]
  (= (first group1) (first group2)))

(defn target-selection
  {:test (fn []
           (is= (target-selection test-state-1)
                {[:immune :group2]    [:infection :group1]
                 [:infection :group2] [:immune :group2]
                 [:immune :group1]    [:infection :group2]
                 [:infection :group1] [:immune :group1]}))}
  [state]
  (let [effective-power-rank (get-target-selection-order state)
        group-ids (get-group-ids state)]
    (->> effective-power-rank
         (reduce (fn [a g]
                   (let [defender (->> (:defender-ids a)
                                       (remove (fn [og] (same-team? g og)))
                                       (map (fn [d] {:id              d
                                                     :damage          (calculate-damage state g d)
                                                     :effective-power (get-effective-power state d)}))
                                       (remove (fn [d] (zero? (:damage d))))
                                       (sort-by (juxt :damage :effective-power) #(compare %2 %1))
                                       (first)
                                       (:id))]
                     (if defender
                       (-> a
                           (update :result assoc g defender)
                           (update :defender-ids disj defender))
                       a)))
                 {:result       {}
                  :defender-ids group-ids})
         (:result))))

(defn attack
  [state attacker-id defender-id]
  (let [attacker (get-group state attacker-id)
        defender (get-group state defender-id)]
    (if-not (and attacker defender)
      state
      (let [killed-units (get-killed-units state attacker-id defender-id)
            existing-units (:units (get-in state defender-id))]
        (if (> existing-units killed-units)
          (update-in state defender-id
                     (fn [defender]
                       (update defender :units - killed-units)))
          (update state (first defender-id)
                  (fn [team] (dissoc team (second defender-id)))))))))


(defn attack-phase
  [state planed-attacks]
  (let [initiative-rank (get-initiative-rank state)]
    (reduce (fn [state attacker-id]
              (attack state attacker-id (get planed-attacks attacker-id)))
            state
            initiative-rank)))

(defn play-a-round
  {:test (fn []
           (is= (play-a-round test-state-1)
                {:immune    {:group2 {:id         [:immune :group2]
                                      :units      905
                                      :hit-points 1274
                                      :immune     #{:fire}
                                      :weakness   #{:bludgeoning :slashing}
                                      :attack     25
                                      :type       :slashing
                                      :initiative 3}}
                 :infection {:group1 {:id         [:infection :group1]
                                      :units      797
                                      :hit-points 4706
                                      :weakness   #{:radiation}
                                      :attack     116
                                      :type       :bludgeoning
                                      :initiative 1}
                             :group2 {:id         [:infection :group2]
                                      :units      4434
                                      :hit-points 2961
                                      :immune     #{:radiation}
                                      :weakness   #{:fire :cold}
                                      :attack     12
                                      :type       :slashing
                                      :initiative 4}}}))}
  [state]
  (let [planed-attacks (target-selection state)]
    (println "planed attacks:" planed-attacks)
    (attack-phase state planed-attacks)))

(defn end?
  [state]
  (or (= (get state :immune) {})
      (= (get state :infections) {})))

(defn get-units
  [state]
  (->> (get-group-ids state)
       (reduce (fn [a v]
                 (assoc a v (:units (get-group state v))))
               {})))

(defn play
  {:test (fn []
           (is= (play test-state-1)
                {:immune    {},
                 :infection {:group1 {:id         [:infection :group1],
                                      :units      782,
                                      :hit-points 4706,
                                      :weakness   #{:radiation},
                                      :attack     116,
                                      :type       :bludgeoning,
                                      :initiative 1},
                             :group2 {:id         [:infection :group2],
                                      :units      4434,
                                      :hit-points 2961,
                                      :immune     #{:radiation},
                                      :weakness   #{:fire :cold},
                                      :attack     12,
                                      :type       :slashing,
                                      :initiative 4}}}))}
  [state]
  (let [state (play-a-round state)]
    (println (get-units state))
    (if (end? state)
      state
      (recur state))))

(def state
  {:immune    {:group0 {:id         [:immune :group0]
                        :units      4445
                        :hit-points 10125
                        :immune     #{:radiation}
                        :attack     20
                        :type       :cold
                        :initiative 16}
               :group1 {:id         [:immune :group1]
                        :units      722
                        :hit-points 9484
                        :attack     130
                        :type       :bludgeoning
                        :initiative 6}
               :group2 {:id         [:immune :group2]
                        :units      1767
                        :hit-points 5757
                        :weak       #{:fire :radiation}
                        :attack     27
                        :type       :radiation
                        :initiative 4}
               :group3 {:id         [:immune :group3]
                        :units      1472
                        :hit-points 7155
                        :weak       #{:slashing :bludgeoning}
                        :attack     42
                        :type       :radiation
                        :initiative 20}
               :group4 {:id         [:immune :group4]
                        :units      2610
                        :hit-points 5083
                        :weak       #{:slashing :fire}
                        :attack     14
                        :type       :fire
                        :initiative 17}
               :group5 {:id         [:immune :group5]
                        :units      442
                        :hit-points 1918
                        :attack     35
                        :type       :fire
                        :initiative 8}
               :group6 {:id         [:immune :group6]
                        :units      2593
                        :hit-points 1755
                        :immune     #{:bludgeoning :radiation :fire}
                        :attack     6
                        :type       :slashing
                        :initiative 13}
               :group7 {:id         [:immune :group7]
                        :units      6111
                        :hit-points 1395
                        :weak       #{:bludgeoning}
                        :immune     #{:radiation :fire}
                        :attack     1
                        :type       :slashing
                        :initiative 14}
               :group8 {:id         [:immune :group8]
                        :units      231
                        :hit-points 3038
                        :immune     #{:radiation}
                        :attack     128
                        :type       :cold
                        :initiative 15}
               :group9 {:id         [:immune :group9]
                        :units      3091
                        :hit-points 6684
                        :weak       #{:radiation}
                        :immune     #{:slashing}
                        :attack     17
                        :type       :cold
                        :initiative 19}}
   :infection {:group0 {:id         [:infection :group0]
                        :units      1929
                        :hit-points 13168
                        :weak       #{:bludgeoning}
                        :attack     13
                        :type       :fire
                        :initiative 7}
               :group1 {:id         [:infection :group1]
                        :units      2143
                        :hit-points 14262
                        :immune     #{:radiation}
                        :attack     12
                        :type       :fire
                        :initiative 10}
               :group2 {:id         [:infection :group2]
                        :units      1380
                        :hit-points 20450
                        :weak       #{:slashing :radiation}
                        :immune     #{:bludgeoning :fire}
                        :attack     28
                        :type       :cold
                        :initiative 12}
               :group3 {:id         [:infection :group3]
                        :units      4914
                        :hit-points 6963
                        :weak       #{:slashing}
                        :immune     #{:fire}
                        :attack     2
                        :type       :cold
                        :initiative 11}
               :group4 {:id         [:infection :group4]
                        :units      1481
                        :hit-points 14192
                        :weak       #{:slashing :fire}
                        :immune     #{:radiation}
                        :attack     17
                        :type       :bludgeoning
                        :initiative 3}
               :group5 {:id         [:infection :group5]
                        :units      58
                        :hit-points 40282
                        :weak       #{:cold :slashing}
                        :attack     1346
                        :type       :radiation
                        :initiative 9}
               :group6 {:id         [:infection :group6]
                        :units      2268
                        :hit-points 30049
                        :immune     #{:cold :slashing :radiation}
                        :attack     24
                        :type       :radiation
                        :initiative 5}
               :group7 {:id         [:infection :group7]
                        :units      3562
                        :hit-points 22067
                        :attack     9
                        :type       :fire
                        :initiative 18}
               :group8 {:id         [:infection :group8]
                        :units      4874
                        :hit-points 37620
                        :immune     #{:bludgeoning}
                        :weak       #{:cold}
                        :attack     13
                        :type       :bludgeoning
                        :initiative 1}
               :group9 {:id         [:infection :group9]
                        :units      4378
                        :hit-points 32200
                        :weak       #{:cold}
                        :attack     10
                        :type       :bludgeoning
                        :initiative 2}}})

(defn puzzle-1
  {:test (fn []
           (is= (puzzle-1 test-state-1)
                5216))}
  [state]
  (let [end-state (play state)
        groups (->> (get-group-ids end-state)
                    (map (fn [id] (get-group end-state id))))]
    (->> groups
         (map :units)
         (apply +))))

(comment
  (time (let [end-state (play state)
              groups (->> (get-group-ids end-state)
                          (map (fn [id] (get-group end-state id))))]
          (->> groups
               (map :units)
               (apply +))))
  ; too high 25410
  )
