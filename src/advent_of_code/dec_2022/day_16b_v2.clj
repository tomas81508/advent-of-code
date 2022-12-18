(ns advent-of-code.dec-2022.day-16b-v2
  (:require [ysera.test :refer [is is-not is= deftest]]
            [advent-of-code.dec-2022.day-16a-v2 :refer [distance-test-map
                                                        distance-map]]
            [clojure.math.combinatorics]))


(def start-state {:minutes            26
                  :current-states     #{{:players        [{:id :AA :done 26}
                                                          {:id :AA :done 26}]
                                         :open-valves    #{}
                                         :total-pressure 0}}
                  :max-total-pressure 0
                  :visits {}})


(defn dead-end?
  {:test (fn []
           (is (dead-end? [{:total-pressure 1603
                            :players        #{{:id :HH, :done 12} {:id :EE, :done 12}}}]
                          {:players        #{{:id :HH, :done 12} {:id :EE, :done 12}}
                           :total-pressure 1400}))
           (is (dead-end? [{:total-pressure 1603
                            :players        #{{:id :HH, :done 12} {:id :EE, :done 12}}}]
                          {:players        #{{:id :HH, :done 5} {:id :EE, :done 12}}
                           :total-pressure 1603}))
           (is-not (dead-end? [{:total-pressure 1603
                                :players        #{{:id :HH, :done 12} {:id :EE, :done 12}}}]
                              {:players        #{{:id :HH, :done 15} {:id :EE, :done 12}}
                               :total-pressure 1303})))}
  [visits state]
  (->> visits
       (some (fn [{tp :total-pressure ps :players}]
               (and (>= tp (:total-pressure state))
                    (let [first-player-state (first (:players state))
                          second-player-state (second (:players state))]
                      (and (some (fn [{id :id d :done}]
                                   (and (= (:id first-player-state) id)
                                        (>= d (:done first-player-state))))
                                 ps)
                           (some (fn [{id :id d :done}]
                                   (and (= (:id second-player-state) id)
                                        (>= d (:done second-player-state))))
                                 ps))))))))


(defn player->player-states
  [distance-map player minutes open-valves]
  (if (not= (:done player) minutes)
    #{player}
    (->> (distance-map (:id player))
         (:walks)
         (keep (fn [[id distance]] (when-not (contains? open-valves id)
                                     {:id id :done (- minutes distance 1)}))))))


(defn get-new-current-state-positions
  [distance-map players open-valves minutes]
  (let [player1 (first players)
        player2 (second players)
        player1-states (player->player-states distance-map player1 minutes open-valves)
        player2-states (player->player-states distance-map player2 minutes open-valves)]
    (if-not (and (not (empty? player1-states))
                 (not (empty? player2-states)))
      #{}
      (->> (clojure.math.combinatorics/cartesian-product player1-states player2-states)
           (map set)
           (remove (fn [positions] (= (count positions) 1)))
           (into #{})))))


(defn do-one-minute
  {:test (fn [])}
  [distance-map state]
  (let [minutes (:minutes state)
        ; Filtering out when actions needed
        {new-action-visits true old-action-visits nil}
        (->> (:current-states state)
             (group-by (fn [{players :players}]
                         (some (fn [{done :done}] (= done minutes))
                               players))))]
    (reduce (fn [state current-state]
              (let [done-valve-ids (->> current-state
                                        (:players)
                                        (filter (fn [{done :done}] (= done minutes)))
                                        (map :id)
                                        (remove (fn [id] (contains? (:open-valves current-state) id))))]
                (if (empty? done-valve-ids)
                  state
                  (let [done-valve-ids (remove (fn [id] (= id :AA)) done-valve-ids)
                        total-pressure-gained (* (->> done-valve-ids
                                                      (map (fn [id] (:flow-rate (distance-map id))))
                                                      (apply +))
                                                 (inc (:minutes state)))
                        new-total-pressure (+ (:total-pressure current-state) total-pressure-gained)
                        new-state-positions (get-new-current-state-positions distance-map
                                                                             (:players current-state)
                                                                             (:open-valves current-state)
                                                                             minutes)
                        new-states (->> new-state-positions
                                        (map (fn [positions]
                                               {:players        positions
                                                :open-valves    (reduce conj (:open-valves current-state) done-valve-ids)
                                                :total-pressure new-total-pressure}))
                                        (remove (fn [s]
                                                  (let [visits (get-in state [:visits
                                                                              (:open-valves s)
                                                                              (->> (:players s)
                                                                                   (map :id)
                                                                                   (into #{}))])]
                                                    (dead-end? visits s)))))]
                    (-> (if (> new-total-pressure (:max-total-pressure state))
                          (assoc state :max-total-pressure new-total-pressure)
                          state)
                        (update :current-states (fn [states] (reduce conj states new-states)))
                        (update :visits
                                (fn [visits]
                                  (reduce (fn [visits state]
                                            (update-in visits
                                                       [(:open-valves state)
                                                        (->> (:players state)
                                                             (map :id)
                                                             (into #{}))]
                                                       conj (select-keys state [:players
                                                                                :total-pressure])))
                                          visits
                                          new-states))))))))
            (-> state
                (update :minutes dec)
                (assoc :current-states (into #{} old-action-visits)))
            new-action-visits)))


(comment
  (loop [state start-state]
    (println (:minutes state))
    (println (count (:current-states state)))
    (if (= (:minutes state) 0)
      (:max-total-pressure state)
      (recur (time (do-one-minute distance-map state)))))
  ; This algorithm took a long time ...
  )





