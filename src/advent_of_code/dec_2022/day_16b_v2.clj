(ns advent-of-code.dec-2022.day-16b-v2
  (:require [ysera.test :refer [is is-not is= deftest]]
            [advent-of-code.dec-2022.day-16a-v2 :refer [distance-test-map
                                                        distance-map]]))


(def start-state {:current-visits #{{:valves         [{:id :AA :minutes 26}
                                                      {:id :AA :minutes 26}]
                                     :open-valves    #{}
                                     :total-pressure 0}}
                  :visits         {}})

(defn do-one-action
  {:test (fn []
           (is= (do-one-action distance-test-map start-state)
                {:current-visits #{{:valves      #{{:id :JJ :minutes 23}
                                                   {:id :AA :minutes 26}}
                                    :open-valves #{:JJ} :total-pressure 567}
                                   {:valves      #{{:id :HH :minutes 20}
                                                   {:id :AA :minutes 26}}
                                    :open-valves #{:HH} :total-pressure 528}
                                   {:valves      #{{:id :BB :minutes 24}
                                                   {:id :AA :minutes 26}}
                                    :open-valves #{:HH} :total-pressure 364}
                                   {:valves      #{{:id :DD :minutes 24}
                                                   {:id :AA :minutes 26}}
                                    :open-valves #{:DD} :total-pressure 560}
                                   {:valves      #{{:id :EE :minutes 23}
                                                   {:id :AA :minutes 26}}
                                    :open-valves #{:EE} :total-pressure 81}
                                   {:valves      #{{:id :CC :minutes 23}
                                                   {:id :AA :minutes 26}}
                                    :open-valves #{:CC} :total-pressure 54}}
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
                                                    conj {:total-pressure (:total-pressure new-visit)
                                                          :minutes        (:minutes new-visit)}))))

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





