(ns advent-of-code.dec-2023.day-20
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :refer [split-lines split starts-with?]]))

(def input (-> (slurp "src/advent_of_code/dec_2023/day_20_input.txt")
               (split-lines)))
(def test-input ["broadcaster -> a"
                 "%a -> inv, con"
                 "&inv -> b"
                 "%b -> con"
                 "&con -> output"])

(defn create-configuration
  {:test (fn []
           (is= (create-configuration test-input)
                {:broadcaster {:destinations [:a]
                               :type         :broadcaster}
                 :a           {:type         :flip-flop
                               :destinations [:inv :con]}
                 :inv         {:type         :conjunction
                               :destinations [:b]}
                 :b           {:type         :flip-flop
                               :destinations [:con]}
                 :con         {:type         :conjunction
                               :destinations [:output]}}))}
  [input]
  (reduce (fn [a line]
            (let [[_ from to] (re-find #"(.+) -> (.+)" line)
                  type (cond (starts-with? from "%") :flip-flop
                             (starts-with? from "&") :conjunction
                             :else :broadcaster)
                  id (keyword (clojure.string/replace from #"(%|&)" ""))
                  destinations (->> (split to #", ")
                                    (map keyword))]
              (assoc a id {:type         type
                           :destinations destinations})))
          {}
          input))

(def configuration (create-configuration input))
(def test-configuration (create-configuration test-input))

(def state {:components {:a   :off
                         :inv :low
                         :b   :off
                         :con :low}
            :signals    [[:broadcaster :low]]})

(defn perform-flip-flop
  [state id pulse]
  (if (= pulse :high)
    [state :high]
    [(-> state
         (update-in [:components id] {:on :off :off :on}))
     (if (= (get-in state [:components id]) :off)
       :high
       :low)]))


(defn send-all-signals
  {:test (fn []
           (is= (send-all-signals test-configuration
                                  {:components {:a   :off
                                                :inv :low
                                                :b   :off
                                                :con :low}
                                   :signals    [[:broadcaster :low]]})
                {:components {:a   :off
                              :inv :low
                              :b   :off
                              :con :low}
                 :signals    [[:a :low]]})
           (is= (send-all-signals test-configuration
                                  {:components {:a   :off
                                                :inv :low
                                                :b   :off
                                                :con :low}
                                   :signals    [[:a :low]]})
                {:components {:a   :on
                              :inv :low
                              :b   :off
                              :con :low}
                 :signals    [[:inv :high] [:con :high]]}))}
  [configuration state]
  (let [[state signals]
        (reduce (fn [[state queued-signals] [id pulse]]
                  (let [type (get-in configuration [id :type])
                        destinations (get-in configuration [id :destinations])]
                    (case type
                      :broadcaster [state (map (fn [d] [d pulse]) destinations)]
                      :flip-flop (let [[state pulse] (perform-flip-flop state id pulse)]
                                   [state (map (fn [d] [d pulse]) destinations)]))))
                [state []]
                (:signals state))]
    (assoc state :signals signals)))

