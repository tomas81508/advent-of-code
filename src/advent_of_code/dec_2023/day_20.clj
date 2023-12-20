(ns advent-of-code.dec-2023.day-20
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]
            [clojure.string :refer [split-lines split starts-with?]]))

; Together with Mattias Liljeström and Anders Lång

(def input (-> (slurp "src/advent_of_code/dec_2023/day_20_input.txt")
               (split-lines)))
(def test-input ["broadcaster -> a"
                 "%a -> inv, con"
                 "&inv -> b"
                 "%b -> con"
                 "&con -> output"])

(def test-input-2 ["broadcaster -> a, b, c"
                   "%a -> b"
                   "%b -> c"
                   "%c -> inv"
                   "&inv -> a"])

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
(def test-configuration-2 (create-configuration test-input-2))

(defn create-state
  {:test (fn []
           (is= (create-state test-configuration)
                {:components {:a   :off
                              :inv :low
                              :b   :off
                              :con :low}
                 :signals    [[:button :low :broadcaster]]
                 :pulses     {:high 0 :low 0}}))}
  [configuration]
  {:components (reduce-kv (fn [a k v]
                            (case (:type v)
                              :flip-flop (assoc a k :off)
                              :conjunction (assoc a k :low)
                              a))
                          {}
                          configuration)
   :signals    [[:button :low :broadcaster]]
   :pulses     {:high 0 :low 0}})

(def state (create-state configuration))
(def test-state (create-state test-configuration))
(def test-state-2 (create-state test-configuration-2))

(defn perform-flip-flop
  [state id pulse]
  (when (= pulse :low)
    [(-> state
         (update-in [:components id] {:on :off :off :on}))
     (if (= (get-in state [:components id]) :off)
       :high
       :low)]))

(defn get-input-ids-to-conjunction
  {:test (fn []
           (is= (get-input-ids-to-conjunction test-configuration :inv)
                [:a])
           (is= (get-input-ids-to-conjunction test-configuration :con)
                [:a :b]))}
  [configuration id]
  (reduce-kv (fn [a k v]
               (if (->> (:destinations v)
                        (some (fn [d] (= d id))))
                 (conj a k)
                 a))
             []
             configuration))

(defn all-inputs-on?
  [state ids]
  (->> ids
       (map (fn [id] (get-in state [:components id])))
       (every? (fn [v] (= v :on)))))

(defn perform-conjunction
  [configuration state id pulse]
  (let [state (assoc-in state [:components id] pulse)
        input-ids-to-conjunction (get-input-ids-to-conjunction configuration id)]
    [state (if (all-inputs-on? state input-ids-to-conjunction) :low :high)]))

(defn send-all-signals
  {:test (fn []
           (is= (send-all-signals test-configuration test-state)
                {:components {:a   :off
                              :inv :low
                              :b   :off
                              :con :low}
                 :signals    [[:broadcaster :low :a]]
                 :pulses     {:high 0 :low 1}})
           (is= (send-all-signals test-configuration
                                  {:components {:a   :off
                                                :inv :low
                                                :b   :off
                                                :con :low}
                                   :signals    [[:broadcaster :low :a]]
                                   :pulses     {:high 0 :low 1}})
                {:components {:a   :on
                              :inv :low
                              :b   :off
                              :con :low}
                 :signals    [[:a :high :inv] [:a :high :con]]
                 :pulses     {:high 0 :low 2}})
           (is= (send-all-signals test-configuration
                                  {:components {:a   :on
                                                :inv :low
                                                :b   :off
                                                :con :low}
                                   :signals    [[:a :high :inv] [:a :high :con]]
                                   :pulses     {:high 0 :low 2}})
                {:components {:a   :on
                              :inv :high
                              :b   :off
                              :con :high}
                 :signals    [[:inv :low :b] [:con :high :output]]
                 :pulses     {:high 2 :low 2}})
           (is= (send-all-signals test-configuration
                                  {:components {:a   :on
                                                :inv :high
                                                :b   :off
                                                :con :high}
                                   :signals    [[:inv :low :b] [:con :high :output]]
                                   :pulses     {:high 2 :low 2}})
                {:components {:a   :on
                              :inv :high
                              :b   :on
                              :con :high}
                 :signals    [[:b :high :con]]
                 :pulses     {:high 3 :low 3}}))}
  [configuration state]
  (let [[state signals]
        (reduce (fn [[state queued-signals] [from pulse to]]
                  ;(println from pulse "->" to)
                  (let [state (update-in state [:pulses pulse] inc)]
                    ;(if (= to :output)
                    ;  [(update-in state [:output pulse] inc) queued-signals]
                    (let [type (get-in configuration [to :type])
                          destinations (get-in configuration [to :destinations])
                          result
                          (case type
                            :broadcaster [state pulse]
                            :flip-flop (perform-flip-flop state to pulse)
                            :conjunction (perform-conjunction configuration state to pulse)
                            (println "Nothing at" to))]
                      (if-not result
                        [state queued-signals]
                        (let [[state pulse] result]
                          (reduce (fn [[state queued-signals] d]
                                    ;(println "CONJ" (conj queued-signals [to pulse d]))
                                    [state (conj queued-signals [to pulse d])])
                                  [state queued-signals]
                                  destinations))))))
                [state []]
                (:signals state))]
    (assoc state :signals signals)))

(defn push-button
  {:test (fn []
           (is= (push-button test-configuration-2 test-state-2)
                {:components {:a :off :b :off :c :off :inv :low}
                 :signals []
                 :pulses {:high 4 :low 8}}))}
  [configuration state]
  (loop [state (assoc state :signals [[:button :low :broadcaster]])]
    (if (empty? (:signals state))
      state
      (recur (send-all-signals configuration state)))))

(defn push-button-n-times
  {:test (fn []
           (is= (push-button-n-times test-configuration test-state 1)
                {:components {:a :on :inv :high :b :on :con :high}
                 :signals    []
                 :pulses     {:high 4 :low 4}})
           (is= (push-button-n-times test-configuration test-state 2)
                {:components {:a :off :inv :low :b :on :con :low}
                 :signals    []
                 :pulses     {:high 6 :low 8}})
           (is= (push-button-n-times test-configuration test-state 3)
                {:components {:a :on :inv :high :b :off :con :low}
                 :signals    []
                 :pulses     {:high 9 :low 13}})
           (is= (push-button-n-times test-configuration test-state 4)
                {:components {:a :off :inv :low :b :off :con :low}
                 :signals    []
                 :pulses     {:high 11 :low 17}}))}
  [configuration state n]
  (reduce (fn [state _]
            ;(println "----- PUSHING THE BUTTON -------")
            (push-button configuration state))
          state
          (range n)))


(deftest test-puzzle-a
  (is= (->> (push-button-n-times test-configuration test-state 1000)
            (:pulses))
       {:high 2750, :low 4250})

  (is= (->> (push-button-n-times test-configuration-2 test-state-2 1000)
            (:pulses))
       {:high 4000, :low 8000})
  )

(comment
  (->> (push-button-n-times configuration state 1000)
       (:pulses)
       (vals)
       (apply *))
  ; 570300420 ;low
  )
