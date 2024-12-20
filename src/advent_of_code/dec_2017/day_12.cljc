(ns advent-of-code.dec-2017.day-12
  (:require [advent-of-code.test :refer [is=]]
            [clojure.set :refer [union difference]]))

(def input (-> (slurp "src/advent_of_code/dec_2017/day_12_input.txt")
               (clojure.string/split-lines)))

(def test-input ["0 <-> 2"
                 "1 <-> 1"
                 "2 <-> 0, 3, 4"
                 "3 <-> 2, 4"
                 "4 <-> 2, 3, 6"
                 "5 <-> 6"
                 "6 <-> 4, 5"])

(defn create-program-pipes
  {:test (fn []
           (is= (create-program-pipes test-input)
                {0 [2]
                 1 [1]
                 2 [0 3 4]
                 3 [2 4]
                 4 [2 3 6]
                 5 [6]
                 6 [4 5]}))}
  [input]
  (reduce (fn [acc line]
            (let [numbers-string (re-seq #"\d+" line)
                  numbers (map read-string numbers-string)
                  k (first numbers)
                  r (rest numbers)]
              (assoc acc k r)))
          {}
          input))

(def test-pipes (create-program-pipes test-input))
(def pipes (create-program-pipes input))

(defn get-group
  [pipes program-id]
  (loop [state {:programs #{program-id}
                :check    #{program-id}}]
    (if (empty? (:check state))
      state
      (let [potential-programs (->> (:check state)
                                    (map (fn [program]
                                           (get pipes program)))
                                    (flatten)
                                    (set))
            new-programs (difference potential-programs
                                     (:programs state))]
        (recur {:programs (union (:programs state)
                                 new-programs)
                :check    new-programs})))))

(comment
  (def result (get-group pipes 0))
  (count (:programs result))
  )

(defn get-groups
  [pipes]
  (loop [state {:program-groups []}]
    (let [added-programs (->> (:program-groups state) (reduce union) (set))
          missing-program-id (first (difference (-> (keys pipes) (set)) added-programs))]
      (if missing-program-id
        (recur (update state :program-groups conj (:programs (get-group pipes missing-program-id))))
        state))))

(comment
  (def result-2 (get-groups pipes))
  (count (:program-groups result-2))
  )
