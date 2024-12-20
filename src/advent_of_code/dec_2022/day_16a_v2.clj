(ns advent-of-code.dec-2022.day-16a-v2
  (:require [advent-of-code.test :refer [is=]]))

(def input (slurp "src/advent_of_code/dec_2022/day_16_input.txt"))
(def test-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II")

(def line-pattern #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)")

(defn input->state
  [input]
  (->> input
       (clojure.string/split-lines)
       (map (fn [l]
              (->> (re-find line-pattern l)
                   (rest))))
       (reduce (fn [a l]
                 (let [valve (first l)
                       flow-rate (second l)
                       tunnels-to (nth l 2)]
                   (assoc a (keyword valve)
                            {:flow-rate  (read-string flow-rate)
                             :tunnels-to (->> (clojure.string/split tunnels-to #", ")
                                              (map keyword)
                                              (into #{}))})))
               {})))

(def test-map (input->state test-input))
(def input-map (input->state input))

(defn get-distance
  {:test (fn []
           (is= (get-distance {:AA {:flow-rate 0, :tunnels-to #{:BB :II :DD}},
                               :BB {:flow-rate 13, :tunnels-to #{:CC :AA}},
                               :CC {:flow-rate 2, :tunnels-to #{:BB :DD}},
                               :DD {:flow-rate 20, :tunnels-to #{:CC :AA :EE}},
                               :EE {:flow-rate 3, :tunnels-to #{:FF :DD}},
                               :FF {:flow-rate 0, :tunnels-to #{:GG :EE}},
                               :GG {:flow-rate 0, :tunnels-to #{:HH :FF}},
                               :HH {:flow-rate 22, :tunnels-to #{:GG}},
                               :II {:flow-rate 0, :tunnels-to #{:AA :JJ}},
                               :JJ {:flow-rate 21, :tunnels-to #{:II}}}
                              :AA
                              :HH)
                5))}
  [the-map source-id destination-id]
  (loop [active-walks #{source-id}
         visited-valves #{source-id}
         distance 0]
    (if (contains? active-walks destination-id)
      distance
      (let [new-valves (clojure.set/difference (->> active-walks
                                                    (map (fn [v] (get-in the-map [v :tunnels-to])))
                                                    (reduce concat)
                                                    (into #{}))
                                               visited-valves)]
        (recur new-valves
               (reduce conj visited-valves new-valves)
               (inc distance))))))

(defn get-distance-part-map
  {:test (fn []
           (is= (get-distance-part-map {:AA {:flow-rate 0, :tunnels-to #{:BB :II :DD}},
                                        :BB {:flow-rate 13, :tunnels-to #{:CC :AA}},
                                        :CC {:flow-rate 2, :tunnels-to #{:BB :DD}},
                                        :DD {:flow-rate 20, :tunnels-to #{:CC :AA :EE}},
                                        :EE {:flow-rate 3, :tunnels-to #{:FF :DD}},
                                        :FF {:flow-rate 0, :tunnels-to #{:GG :EE}},
                                        :GG {:flow-rate 0, :tunnels-to #{:HH :FF}},
                                        :HH {:flow-rate 22, :tunnels-to #{:GG}},
                                        :II {:flow-rate 0, :tunnels-to #{:AA :JJ}},
                                        :JJ {:flow-rate 21, :tunnels-to #{:II}}}
                                       :AA)
                {:BB 1 :CC 2 :DD 1 :EE 2 :HH 5 :JJ 2}))}
  [the-map valve-id]
  (reduce (fn [a v] (assoc a v (get-distance the-map valve-id v)))
          {}
          (->> (keys the-map)
               (filter (fn [k]
                         (and (not= k valve-id)
                              (let [destination-valve (get the-map k)]
                                (pos? (:flow-rate destination-valve)))))))))

(defn create-distance-map
  {:test (fn []
           (is= (create-distance-map {:AA {:flow-rate 0, :tunnels-to #{:BB :II :DD}},
                                      :BB {:flow-rate 13, :tunnels-to #{:CC :AA}},
                                      :CC {:flow-rate 2, :tunnels-to #{:BB :DD}},
                                      :DD {:flow-rate 20, :tunnels-to #{:CC :AA :EE}},
                                      :EE {:flow-rate 3, :tunnels-to #{:FF :DD}},
                                      :FF {:flow-rate 0, :tunnels-to #{:GG :EE}},
                                      :GG {:flow-rate 0, :tunnels-to #{:HH :FF}},
                                      :HH {:flow-rate 22, :tunnels-to #{:GG}},
                                      :II {:flow-rate 0, :tunnels-to #{:AA :JJ}},
                                      :JJ {:flow-rate 21, :tunnels-to #{:II}}})
                {:AA {:flow-rate 0
                      :walks     {:BB 1 :CC 2 :DD 1 :EE 2 :HH 5 :JJ 2}}
                 :BB {:flow-rate 13
                      :walks     {:CC 1 :DD 2 :EE 3 :HH 6 :JJ 3}}
                 :CC {:flow-rate 2
                      :walks     {:BB 1 :DD 1 :EE 2 :HH 5 :JJ 4}}
                 :DD {:flow-rate 20
                      :walks     {:BB 2 :CC 1 :EE 1 :HH 4 :JJ 3}}
                 :EE {:flow-rate 3
                      :walks     {:BB 3 :CC 2 :DD 1 :HH 3 :JJ 4}}
                 :HH {:flow-rate 22
                      :walks     {:BB 6 :CC 5 :DD 4 :EE 3 :JJ 7}}
                 :JJ {:flow-rate 21
                      :walks     {:BB 3 :CC 4 :DD 3 :EE 4 :HH 7}}}))}
  [the-map]
  (reduce (fn [state valve-id]
            (let [valve (get the-map valve-id)]
              (if (and (zero? (:flow-rate valve))
                       (not= valve-id :AA))
                state
                (assoc state valve-id {:flow-rate (:flow-rate valve)
                                       :walks     (get-distance-part-map the-map valve-id)}))))
          {}
          (keys the-map)))

(def distance-test-map (create-distance-map test-map))
(def distance-map (create-distance-map input-map))

(def start-state {:current-visits #{{:id             :AA
                                     :open-valves    #{}
                                     :total-pressure 0
                                     :minutes        30}}
                  :visits         {}})

(defn been-here-before?
  [valve visits]
  (->> visits
       (some (fn [v]
               (and (<= (:total-pressure valve) (:total-pressure v))
                    (>= (:minutes valve) (:minutes v)))))))

(defn do-one-action
  {:test (fn []
           (is= (do-one-action distance-test-map start-state)
                {:current-visits #{{:id :JJ :open-valves #{:JJ} :total-pressure 567 :minutes 27}
                                   {:id :HH :open-valves #{:HH} :total-pressure 528 :minutes 24}
                                   {:id :BB :open-valves #{:BB} :total-pressure 364 :minutes 28}
                                   {:id :DD :open-valves #{:DD} :total-pressure 560 :minutes 28}
                                   {:id :EE :open-valves #{:EE} :total-pressure 81 :minutes 27}
                                   {:id :CC :open-valves #{:CC} :total-pressure 54 :minutes 27}}
                 :visits         {:CC {#{:CC} [{:total-pressure 54 :minutes 27}]}
                                  :HH {#{:HH} [{:total-pressure 528 :minutes 24}]}
                                  :BB {#{:BB} [{:total-pressure 364 :minutes 28}]}
                                  :EE {#{:EE} [{:total-pressure 81 :minutes 27}]}
                                  :DD {#{:DD} [{:total-pressure 560 :minutes 28}]}
                                  :JJ {#{:JJ} [{:total-pressure 567 :minutes 27}]}}}))}
  [distance-map state]
  (reduce (fn [state current-visit]
            (let [destinations (distance-map (:id current-visit))]
              ; destinations = {:flow-rate 0, :walks {:CC 2, :HH 5, :BB 1, :EE 2, :DD 1, :JJ 2}}
              (reduce-kv (fn [state destination distance]
                           (cond (contains? (:open-valves current-visit) destination)
                                 state

                                 (< distance (:minutes current-visit))
                                 (let [new-minutes (- (:minutes current-visit) distance 1)
                                       new-flow-rate (:flow-rate (distance-map destination))
                                       new-visit (-> current-visit
                                                     (assoc :id destination
                                                            :minutes new-minutes)
                                                     (update :open-valves conj destination)
                                                     (update :total-pressure + (* new-minutes new-flow-rate)))]
                                   (if (been-here-before? new-visit
                                                          (get-in state [:visits destination (:open-valves new-visit)]))
                                     state
                                     (-> state
                                         (update :current-visits conj new-visit)
                                         (update-in [:visits destination (:open-valves new-visit)]
                                                    (fn [stored-values]
                                                      (let [new-total-pressure (:total-pressure new-visit)]
                                                        (conj (->> stored-values
                                                                   (remove (fn [{m :minutes tp :total-pressure}]
                                                                             (and (<= m new-minutes)
                                                                                  (<= tp new-total-pressure)))))
                                                              {:total-pressure new-total-pressure
                                                               :minutes        new-minutes})))))))

                                 :else state))
                         state
                         (:walks destinations))))
          (-> state
              (assoc :current-visits #{}))
          (:current-visits state)))


(comment

  (time (->> (loop [state start-state]
               (if (empty? (:current-visits state))
                 state
                 (recur (do-one-action distance-test-map state))))
             (:visits)
             (vals)
             (map vals)
             (flatten)
             (map :total-pressure)
             (reduce max)))
  ; "Elapsed time: 5.65798 msecs"
  ; => 1651

  (time (->> (loop [state start-state]
               (println (count (:current-visits state)))
               (if (empty? (:current-visits state))
                 state
                 (recur (do-one-action distance-map state))))
             (:visits)
             (vals)
             (map vals)
             (flatten)
             (map :total-pressure)
             (reduce max)))
  ; "Elapsed time: 680.057434 msecs"
  ; => 1673

  )





