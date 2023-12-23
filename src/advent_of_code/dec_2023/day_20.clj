(ns advent-of-code.dec-2023.day-20
  (:require [advent-of-code.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains?]]
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
                {:components     {:a   :off
                                  :inv {:a :low}
                                  :b   :off
                                  :con {:a :low :b :low}}
                 :signals        [[:button :low :broadcaster]]
                 :pulses         {:high 0 :low 0}
                 :ll-high        {}
                 :button-pressed 0
                 :test-output    {:high 0 :low 0}}))}
  [configuration]
  {:components     (reduce-kv (fn [a k v]
                                (case (:type v)
                                  :flip-flop (assoc a k :off)
                                  :conjunction
                                  (assoc a k (reduce-kv
                                               (fn [a id {destinations :destinations}]
                                                 (if (seq-contains? destinations k)
                                                   (assoc a id :low)
                                                   a))
                                               {}
                                               configuration))
                                  a))
                              {}
                              configuration)
   :signals        [[:button :low :broadcaster]]
   :pulses         {:high 0 :low 0}
   :ll-high        {}
   :button-pressed 0
   :test-output    {:high 0 :low 0}})

(def state (create-state configuration))
(def test-state (create-state test-configuration))
(def test-state-2 (create-state test-configuration-2))

(defn perform-flip-flop
  [state id pulse]
  (if (= pulse :high)
    [state nil]
    [(-> state
         (update-in [:components id] {:on :off :off :on}))
     (if (= (get-in state [:components id]) :off)
       :high
       :low)]))

(defn perform-conjunction
  [state id from pulse]
  (let [state (assoc-in state [:components id from] pulse)
        new-pulse (if (->> (get-in state [:components id]) (vals) (every? #{:high})) :low :high)]
    [(if (and (= pulse :high) (= id :ll))
       (update-in state [:ll-high from] conj (:button-pressed state))
       state)
     new-pulse]))

(defn send-all-signals
  {:test (fn []
           (is= (send-all-signals test-configuration test-state)
                {:components  {:a   :off
                               :inv {:a :low}
                               :b   :off
                               :con {:a :low :b :low}}
                 :signals     [[:broadcaster :low :a]]
                 :pulses      {:high 0 :low 1}
                 :ll-high     {}
                 :test-output {:high 0 :low 0}})
           (is= (send-all-signals test-configuration
                                  {:components  {:a   :off
                                                 :inv {:a :low}
                                                 :b   :off
                                                 :con {:a :low :b :low}}
                                   :signals     [[:broadcaster :low :a]]
                                   :pulses      {:high 0 :low 1}
                                   :ll-high     {}
                                   :test-output {:high 0 :low 0}})
                {:components  {:a   :on
                               :inv {:a :low}
                               :b   :off
                               :con {:a :low :b :low}}
                 :signals     [[:a :high :inv] [:a :high :con]]
                 :pulses      {:high 0 :low 2}
                 :ll-high     {}
                 :test-output {:high 0 :low 0}})
           (is= (send-all-signals test-configuration
                                  {:components  {:a   :on
                                                 :inv {:a :low}
                                                 :b   :off
                                                 :con {:a :low :b :low}}
                                   :signals     [[:a :high :inv] [:a :high :con]]
                                   :pulses      {:high 0 :low 2}
                                   :ll-high     {}
                                   :test-output {:high 0 :low 0}})
                {:components  {:a   :on
                               :inv {:a :high}
                               :b   :off
                               :con {:a :high :b :low}}
                 :signals     [[:inv :low :b] [:con :high :output]]
                 :pulses      {:high 2 :low 2}
                 :ll-high     {}
                 :test-output {:high 0 :low 0}})
           (is= (send-all-signals test-configuration
                                  {:components  {:a   :on
                                                 :inv {:a :high}
                                                 :b   :off
                                                 :con {:a :high :b :low}}
                                   :signals     [[:inv :low :b] [:con :high :output]]
                                   :pulses      {:high 2 :low 2}
                                   :ll-high     {}
                                   :test-output {:high 0 :low 0}})
                {:components  {:a   :on
                               :inv {:a :high}
                               :b   :on
                               :con {:a :high :b :low}}
                 :signals     [[:b :high :con]]
                 :pulses      {:high 3 :low 3}
                 :ll-high     {}
                 :test-output {:high 1 :low 0}}))}
  [configuration state]
  (let [[state signals]
        (reduce (fn [[state queued-signals] [from pulse to]]
                  ;(println from pulse "->" to)
                  (let [state (update-in state [:pulses pulse] inc)
                        type (get-in configuration [to :type])
                        destinations (get-in configuration [to :destinations])
                        [state pulse] (case type
                                        :broadcaster [state pulse]
                                        :flip-flop (perform-flip-flop state to pulse)
                                        :conjunction (perform-conjunction state to from pulse)
                                        [(update-in state [:test-output pulse] inc) nil])]
                    (reduce (fn [[state queued-signals] d]
                              [state (if pulse (conj queued-signals [to pulse d]) queued-signals)])
                            [state queued-signals]
                            destinations)))
                [state []]
                (:signals state))]
    (assoc state :signals signals)))

(defn push-button
  {:test (fn []
           (is= (push-button test-configuration-2 test-state-2)
                {:components     {:a :off :b :off :c :off :inv {:c :low}}
                 :signals        []
                 :pulses         {:high 4 :low 8}
                 :button-pressed 0
                 :ll-high        {}
                 :test-output    {:high 0 :low 0}}))}
  [configuration state]
  (loop [state (-> state
                   (assoc :signals [[:button :low :broadcaster]])
                   (update :button-pressed inc))]
    (if (empty? (:signals state))
      state
      (recur (send-all-signals configuration state)))))

(defn push-button-n-times
  {:test (fn []
           (is= (push-button-n-times test-configuration test-state 1)
                {:components  {:a :on, :inv {:a :high}, :b :on, :con {:a :high, :b :high}}
                 :signals     []
                 :pulses      {:high 4 :low 4}
                 :ll-high     {}
                 :test-output {:high 1 :low 1}})
           (is= (push-button-n-times test-configuration test-state 2)
                {:components  {:a :off, :inv {:a :low}, :b :on, :con {:a :low, :b :high}}
                 :signals     []
                 :pulses      {:high 6 :low 8}
                 :ll-high     {}
                 :test-output {:high 2, :low 1}})
           (is= (push-button-n-times test-configuration test-state 3)
                {:components  {:a :on, :inv {:a :high}, :b :off, :con {:a :high, :b :low}}
                 :signals     []
                 :pulses      {:high 9 :low 13}
                 :ll-high     {}
                 :test-output {:high 3, :low 2}})
           (is= (push-button-n-times test-configuration test-state 4)
                {:components  {:a :off, :inv {:a :low}, :b :off, :con {:a :low, :b :low}}
                 :signals     []
                 :pulses      {:high 11 :low 17}
                 :ll-high     {}
                 :test-output {:high 4, :low 2}}))}
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

(deftest puzzle-a
  (is= (time (->> (push-button-n-times configuration state 1000)
                  (:pulses)
                  (vals)
                  (apply *)))
       ; "Elapsed time: 81.141971 msecs"
       743090292))

; :high signal to :rx comes from :ll

; :high signal to :ll conjunction from :kl :kv :vm :vb

; :high signal to :kl comes from :ff
; :high signal to :ff comes from :ld :mb :rs

; :ll {:kl :low, :kv :low, :vm :low, :vb :low},
; want :ll to send high
; ALL :kl, :kv, :vm, :vb must send :high

(comment
  ; Solved with help from Daniel Gullberg

  (def state-10000 (loop [state state
                          i 1]
                     (let [state (push-button configuration state)]
                       (if (< i 10000)
                         (recur state (inc i))
                         state))))
  (->> state-10000
       (:ll-high)
       (vals)
       (map (partial apply -))
       (reduce *))
  )



