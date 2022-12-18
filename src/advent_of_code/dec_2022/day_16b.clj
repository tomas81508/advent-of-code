(ns advent-of-code.dec-2022.day-16b
  (:require [advent-of-code.dec-2022.day-16 :refer [test-map
                                                    puzzle-map]]
            [ysera.test :refer [is is-not is= deftest]]))


(defn dead-end?
  {:test (fn []
           (is (dead-end? {:minute 25 :total-pressure 0}
                          [{:minute 28 :total-pressure 0}]))
           (is (dead-end? {:minute 25 :total-pressure 0}
                          [{:minute 28 :total-pressure 10}]))
           (is-not (dead-end? {:minute 25 :total-pressure 0}
                              [{:minute 22 :total-pressure 10}]))
           (is-not (dead-end? {:minute 25 :total-pressure 30}
                              [{:minute 28 :total-pressure 10}])))}
  [valves visits-at-valves]
  (some (fn [visit]
          (and (<= (:minute valves) (:minute visit))
               (<= (:total-pressure valves) (:total-pressure visit))))
        visits-at-valves))

(defn get-new-valves
  {:test (fn []
           (is= (get-new-valves test-map [{:id :AA :last-action :BB}
                                          {:id :AA :last-action nil}])
                #{#{:BB :II} #{:II} #{:II :DD} #{:BB :DD} #{:DD}}))}
  [the-map [{id-a :id last-action-a :last-action}
            {id-b :id last-action-b :last-action}]]
  (let [tunnels-to-a (disj (:tunnels-to (get test-map :AA)) last-action-a)
        tunnels-to-b (disj (:tunnels-to (get the-map id-b)) last-action-b)]
    (->> (for [v-a tunnels-to-a v-b tunnels-to-b]
           (if (= v-a v-b) #{v-a} #{v-a v-b}))
         (into #{}))))

(def start-state {:minutes      26
                  :active-walks #{{:valves         [{:id          :AA
                                                     :last-action nil}
                                                    {:id          :AA
                                                     :last-action nil}]
                                   :open-valves    #{}
                                   :total-pressure 0}}
                  ; valves -> open-pressures -> data
                  :visits       {#{:AA,,} {#{} [{:total-pressure 0 :minute 30}]}}})


(defn do-a-minute
  {:test (fn []
           (is= (do-a-minute test-map start-state)
                {:minutes      29
                 :active-walks #{{:id :BB :last-action :AA :open-valves #{} :total-pressure 0}
                                 {:id :DD :last-action :AA :open-valves #{} :total-pressure 0}
                                 {:id :II :last-action :AA :open-valves #{} :total-pressure 0}}
                 :visits       {#{:AA}     {#{} [{:total-pressure 0 :minute 30}]}
                                #{:BB}     {#{} [{:total-pressure 0 :minute 29}]}
                                #{:BB :DD} {#{} [{:total-pressure 0 :minute 29}]}
                                #{:BB :II} {#{} [{:total-pressure 0 :minute 29}]}
                                #{:II}     {#{} [{:total-pressure 0 :minute 29}]}
                                #{:II :DD} {#{} [{:total-pressure 0 :minute 29}]}
                                #{:DD}     {#{} [{:total-pressure 0 :minute 29}]}}}))}
  [the-map state]
  (let [minute (dec (:minutes state))]
    (reduce (fn [state valves]
              (reduce (fn [state new-valve-ids]
                        (if (dead-end? {:minute         minute
                                        :total-pressure (:total-pressure valves)}
                                       (get-in state [:visits new-valve-ids (:open-valves valves)]))
                          state
                          (-> state
                              (update-in [:visits new-valve-ids (:open-valves valves)]
                                         (fn [visits]
                                           (let [new-visit {:total-pressure (:total-pressure valve)
                                                            :minute         minute}]
                                             (-> (remove (fn [{tp :total-pressure m :minute}]
                                                           (and (< tp (:total-pressure new-visit))
                                                                (>= m (:minute new-visit))))
                                                         visits)
                                                 (conj new-visit)))))
                              (update :active-walks conj
                                      {:id             new-valve-id
                                       :last-action    (:id valve)
                                       :open-valves    (:open-valves valve)
                                       :total-pressure (:total-pressure valve)}))))
                      (if (and (pos? flow-rate)
                               (not (contains? (:open-valves valve) (:id valve))))
                        (let [valve-after-open (-> (select-keys valve [:open-valves :total-pressure])
                                                   (update :open-valves conj (:id valve))
                                                   (update :total-pressure + (* minute flow-rate)))]
                          (-> state
                              (update :active-walks conj (-> valve-after-open
                                                             (assoc :id (:id valve) :last-action :opening)))
                              (update-in [:visits (:id valve) (:open-valves valve-after-open)]
                                         conj (-> valve-after-open
                                                  (assoc :minute minute)
                                                  (dissoc :open-valves)))))
                        state)
                      (get-new-valves the-map (:valves valves))))
            (-> state
                (update :minutes dec)
                (assoc :active-walks #{}))
            (:active-walks state))))


(comment
  (time
    (->> (reduce (fn [state i]
                   (println i)
                   (if (empty? (:active-walks state))
                     (reduced state)
                     (do-a-minute test-map state)))
                 start-state
                 (range 30))
         (:visits)
         (vals)
         (map vals)
         (flatten)
         (map :total-pressure)
         (reduce max)))

  (time
    (->> (reduce (fn [state i]
                   (println i)
                   (if (empty? (:active-walks state))
                     (reduced state)
                     (do-a-minute puzzle-map state)))
                 start-state
                 (range 30))
         (:visits)
         (vals)
         (map vals)
         (flatten)
         (map :total-pressure)
         (reduce max)))
  ; "Elapsed time: 2085.40062 msecs"
  ; => 1673
  )

