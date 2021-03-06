(ns advent-of-code.dec-2016.day-10
  (:require [ysera.test :refer [deftest is is-not is=]]
            [ysera.collections :refer [seq-contains?]]
            [clojure.string :refer [split-lines starts-with? join]]))

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2016/day_10_input.txt")
      (split-lines)))

(defn create-state
  {:test (fn []
           (is= (create-state ["value 5 goes to bot 2"
                               "bot 2 gives low to bot 1 and high to bot 0"
                               "value 3 goes to bot 1"
                               "bot 1 gives low to output 1 and high to bot 0"
                               "bot 0 gives low to output 2 and high to output 0"
                               "value 2 goes to bot 2"])
                {:bots   {0 {:chips []
                             :low   [:output 2]
                             :high  [:output 0]}
                          1 {:chips [3]
                             :low   [:output 1]
                             :high  [:bot 0]}
                          2 {:chips [2 5]
                             :low   [:bot 1]
                             :high  [:bot 0]}}
                 :output {}}))}
  [instructions]
  (let [value-pattern (re-pattern "value ([\\d]+) goes to bot ([\\d]+)")
        give-pattern (re-pattern "bot ([\\d]+) gives low to ([^ ]+) ([\\d]+) and high to ([^ ]+) ([\\d]+)")]
    (->> instructions
         (reduce (fn [a i]
                   (if (starts-with? i "value")
                     (let [[_ chip-as-string bot-as-string] (re-find value-pattern i)
                           chip (read-string chip-as-string)
                           bot (read-string bot-as-string)]
                       (update-in a [:bots bot :chips] conj chip))
                     (let [[_ bot-as-string
                            low-type-as-string low-value-as-string
                            high-type-as-string high-value-as-string] (re-find give-pattern i)
                           bot (read-string bot-as-string)
                           low-type (keyword (read-string low-type-as-string))
                           low-value (read-string low-value-as-string)
                           high-type (keyword (read-string high-type-as-string))
                           high-value (read-string high-value-as-string)]
                       (-> a
                           (update-in [:bots bot :chips] (fn [chips] (or chips [])))
                           (assoc-in [:bots bot :low] [low-type low-value])
                           (assoc-in [:bots bot :high] [high-type high-value])))))
                 {:output {}}))))

(def test-state (create-state ["value 5 goes to bot 2"
                               "bot 2 gives low to bot 1 and high to bot 0"
                               "value 3 goes to bot 1"
                               "bot 1 gives low to output 1 and high to bot 0"
                               "bot 0 gives low to output 2 and high to output 0"
                               "value 2 goes to bot 2"]))

(defn get-bot-with-two-chips
  {:test (fn []
           (is= (get-bot-with-two-chips test-state)
                2))}
  [state]
  (->> (:bots state)
       (seq)
       (filter (fn [[_ {chips :chips}]] (= (count chips) 2)))
       (map first)
       (first)))

(defn add-chip
  {:test (fn []
           (is= (-> (add-chip test-state [:bot 0] 42)
                    (get-in [:bots 0 :chips]))
                [42])
           (is= (-> (add-chip test-state [:output 0] 42)
                    (get-in [:output 0]))
                42))}
  [state [type n] chip]
  (if (= type :bot)
    (update-in state [:bots n :chips] conj chip)
    (assoc-in state [:output n] chip)))

(defn do-instructions
  {:test (fn []
           (is= (-> (do-instructions test-state)
                    (:output))
                {0 5
                 1 2
                 2 3})
           (is= (do-instructions test-state [2 5])
                2))}
  ([state]
   (do-instructions state nil))
  ([state compare-chips]
   (let [bot (get-bot-with-two-chips state)
         {chips     :chips
          low-data  :low
          high-data :high} (get-in state [:bots bot])
         [low-chip high-chip :as sorted-chips] (sort chips)]
     (cond (= sorted-chips compare-chips) bot

           (not bot) state

           :else
           (recur (-> state
                      (assoc-in [:bots bot :chips] [])
                      (add-chip low-data low-chip)
                      (add-chip high-data high-chip))
                  compare-chips)))))

(deftest puzzle-a
         (is= (-> (get-puzzle-input)
                  (create-state)
                  (do-instructions [17 61]))
              141))

(deftest puzzle-b
         (is= (let [output (->> (get-puzzle-input)
                                (create-state)
                                (do-instructions)
                                (:output))]
                (* (get output 0) (get output 1) (get output 2)))
              1209))