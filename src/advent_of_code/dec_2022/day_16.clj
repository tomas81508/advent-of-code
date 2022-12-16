(ns advent-of-code.dec-2022.day-16
  (:require [ysera.test :refer [is is-not is= deftest]]))

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

(def puzzle-map (input->state input))

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
  [valve visits-at-valve]
  (some (fn [visit]
          (and (<= (:minute valve) (:minute visit))
               (<= (:total-pressure valve) (:total-pressure visit))))
        visits-at-valve))

(def start-state {:minutes      30
                  :active-walks #{{:id :AA :last-action nil :open-valves #{} :total-pressure 0}}
                  :visits       {:AA {#{} [{:total-pressure 0 :minute 30}]}}})

(defn do-a-minute
  {:test (fn []
           (is= (do-a-minute test-map start-state)
                {:minutes      29
                 :active-walks #{{:id :BB :last-action :AA :open-valves #{} :total-pressure 0}
                                 {:id :DD :last-action :AA :open-valves #{} :total-pressure 0}
                                 {:id :II :last-action :AA :open-valves #{} :total-pressure 0}}
                 :visits       {:AA {#{} [{:total-pressure 0 :minute 30}]}
                                :BB {#{} [{:total-pressure 0 :minute 29}]}
                                :DD {#{} [{:total-pressure 0 :minute 29}]}
                                :II {#{} [{:total-pressure 0 :minute 29}]}}}))}
  [the-map state]
  (let [minute (dec (:minutes state))]
    (reduce (fn [state valve]
              (let [{tunnels-to :tunnels-to flow-rate :flow-rate} (get the-map (:id valve))]
                (reduce (fn [state new-valve-id]
                          (if (or (= (:last-action valve) new-valve-id)
                                  (dead-end? {:minute         minute
                                              :open-valves    (:open-valves valve)
                                              :total-pressure (:total-pressure valve)}
                                             (get-in state [:visits new-valve-id (:open-valves valve)])))
                            state
                            (-> state
                                (update-in [:visits new-valve-id (:open-valves valve)]
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
                        tunnels-to)))
            (-> state
                (update :minutes dec)
                (assoc :active-walks #{}))
            (:active-walks state))))

(def test-result
  (reduce (fn [state i]
            (println i)
            (if (empty? (:active-walks state))
              (reduced state)
              (do-a-minute test-map state)))
          start-state
          (range 30)))

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

