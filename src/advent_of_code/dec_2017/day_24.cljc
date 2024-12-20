(ns advent-of-code.dec-2017.day-24
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :refer [split split-lines]]))

(def input (-> (slurp "src/advent_of_code/dec_2017/day_24_input.txt")
               (split-lines)))

(def test-input ["0/2" "2/2" "2/3" "3/4" "3/5" "0/1" "10/1" "9/10"])

(defn input->pieces
  [input]
  (->> input
       (map (fn [s] (->> (split s #"/")
                         (map read-string))))
       (into #{})))

(def test-pieces (input->pieces test-input))
(def pieces (input->pieces input))

(defn start-bridges
  {:test (fn []
           (is= (start-bridges test-pieces)
                {:active-bridges [{:bridge        [[0 2]]
                                   :active-number 2
                                   :pieces-left   #{[2 2] [2 3] [3 4] [10 1] [9 10] [3 5] [0 1]}}
                                  {:bridge        [[0 1]]
                                   :active-number 1
                                   :pieces-left   #{[2 2] [2 3] [3 4] [10 1] [0 2] [9 10] [3 5]}}]
                 :done-bridges   []}))}
  [pieces]
  {:active-bridges (->> pieces
                        (reduce (fn [state p]
                                  (if (or (zero? (first p)) (zero? (second p)))
                                    (conj state {:bridge        [p]
                                                 :active-number (if (zero? (first p)) (second p) (first p))
                                                 :pieces-left   (disj pieces p)})
                                    state))
                                []))
   :done-bridges   []})

(defn adds-to-bridge
  {:test (fn []
           (is= (adds-to-bridge {:bridge        [[0 2]]
                                 :active-number 2
                                 :pieces-left   #{[2 2] [2 3] [3 4] [10 1] [9 10] [3 5] [0 1]}})
                [{:bridge        [[0 2] [2 2]]
                  :active-number 2
                  :pieces-left   #{[2 3] [3 4] [10 1] [9 10] [3 5] [0 1]}}
                 {:bridge        [[0 2] [2 3]]
                  :active-number 3
                  :pieces-left   #{[2 2] [3 4] [10 1] [9 10] [3 5] [0 1]}}]))}
  [{bridge        :bridge
    active-number :active-number
    pieces-left   :pieces-left
    :as           bridge-state}]
  (reduce (fn [bridge-states p]
            (if (or (= (first p) active-number) (= (second p) active-number))
              (conj bridge-states {:bridge        (conj bridge p)
                                   :active-number (if (= (first p) active-number) (second p) (first p))
                                   :pieces-left   (disj pieces-left p)})
              bridge-states))
          []
          pieces-left))

(defn build
  {:test (fn []
           (is= (build {:active-bridges [{:bridge        [[0 2]]
                                          :active-number 2
                                          :pieces-left   #{[2 2] [2 3] [3 4] [10 1] [9 10] [3 5] [0 1]}}
                                         {:bridge        [[0 1]]
                                          :active-number 1
                                          :pieces-left   #{[2 2] [2 3] [3 4] [10 1] [0 2] [9 10] [3 5]}}]
                        :done-bridges   []})
                {:active-bridges [{:bridge [[0 2] [2 2]], :active-number 2, :pieces-left #{[2 3] [3 4] [10 1] [9 10] [3 5] [0 1]}}
                                  {:bridge [[0 2] [2 3]], :active-number 3, :pieces-left #{[2 2] [3 4] [10 1] [9 10] [3 5] [0 1]}}
                                  {:bridge [[0 1] [10 1]], :active-number 10, :pieces-left #{[2 2] [2 3] [3 4] [0 2] [9 10] [3 5]}}],
                 :done-bridges   []}
                ))}
  [state]
  (reduce (fn [state active-bridge]
            (let [new-bridges (adds-to-bridge active-bridge)]
              (if (empty? new-bridges)
                (update state :done-bridges conj active-bridge)
                (apply update state :active-bridges conj new-bridges))))
          (assoc state :active-bridges [])
          (:active-bridges state)))

(comment
  (->> (loop [state (start-bridges test-pieces)]
         (if (empty? (:active-bridges state))
           state
           (let [new-state (build state)]
             (recur new-state))))
       (:done-bridges)
       (map :bridge)
       (map flatten)
       (map (fn [ns] (reduce + ns)))
       (reduce max))

  (time
    (->> (loop [state (start-bridges pieces)]
           (if (empty? (:active-bridges state))
             state
             (let [new-state (build state)]
               (recur new-state))))
         (:done-bridges)
         (map :bridge)
         (map flatten)
         (map (fn [ns] (reduce + ns)))
         (reduce max)))

  ; "Elapsed time: 11122.466177 msecs"
  ; => 1859

  ; part 2
  (def done-bridges (->> (loop [state (start-bridges pieces)]
                           (if (empty? (:active-bridges state))
                             state
                             (let [new-state (build state)]
                               (recur new-state))))
                         (:done-bridges)
                         (map :bridge)))

  (->> done-bridges
       (reduce (fn [{length :length strength :strength :as a} bridge]
                 (let [bridge-length (count bridge)]
                   (cond (> bridge-length length)
                         {:length   bridge-length
                          :strength (->> (flatten bridge)
                                         (reduce +))}

                         (< bridge-length length)
                         a

                         :else
                         (let [bridge-strength (->> (flatten bridge)
                                                    (reduce +))]
                           {:length   bridge-length
                            :strength (max bridge-strength strength)}))))
               {:length   0
                :strength 0}))

  )

