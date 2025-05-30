(ns advent-of-code.dec-2015.day-17
  (:require [advent-of-code.test :refer [is=]]
            [clojure.edn :as edn]))

(def input [33 14 18 20 45 35 16 35 1 13 18 13 50 44 48 6 24 41 30 42])

(def test-input [20 15 10 5 5])

(defn create-state
  {:test (fn []
           (is= (create-state test-input 25)
                {:ongoings     [{:numbers   []
                                 :available test-input
                                 :sum       0}]
                 :combinations []
                 :total        25}))}
  [numbers total]
  {:ongoings     [{:numbers   []
                   :available numbers
                   :sum       0}]
   :combinations []
   :total        total})

(defn get-candidates
  {:test (fn []
           (is= (get-candidates {:numbers   []
                                 :available [20 15 10 5 5]
                                 :sum       0})
                [{:numbers   [20]
                  :available [15 10 5 5]
                  :sum       20}
                 {:numbers   []
                  :available [15 10 5 5]
                  :sum       0}]))}
  [ongoing]
  (let [n (first (:available ongoing))]
    [(-> ongoing
         (update :numbers conj n)
         (update :available (partial drop 1))
         (update :sum + n))
     (-> ongoing
         (update :available (partial drop 1)))]))

(defn step
  {:test (fn []
           (is= (step (create-state test-input 25))
                {:ongoings     [{:numbers   [20]
                                 :available [15 10 5 5]
                                 :sum       20}
                                {:numbers   []
                                 :available [15 10 5 5]
                                 :sum       0}]
                 :combinations []
                 :total        25}))}
  [state]
  (let [candidates (->> (:ongoings state)
                        (map get-candidates)
                        (reduce (fn [a v] (apply conj a v)) [])
                        (filter (fn [{sum :sum}] (<= sum (:total state)))))
        combinations (->> candidates
                          (filter (fn [{sum :sum}] (= sum (:total state)))))
        ongoing (->> candidates
                     (filter (fn [{sum :sum}] (< sum (:total state))))
                     (remove (fn [{available :available}] (empty? available))))]
    (-> state
        (assoc :ongoings ongoing)
        (update :combinations (fn [c] (apply conj c combinations))))))

(defn find-combinations
  [state]
  (if (empty? (:ongoings state))
    (:combinations state)
    (recur (step state))))

(comment
  (def combinations (find-combinations (create-state input 150)))
  (count combinations)
  )

; part 2

(comment
  (def min-number-of-containers (->> combinations
                                     (map :numbers)
                                     (map count)
                                     (reduce min)))

  (def min-count (->> combinations
                      (filter (fn [{numbers :numbers}] (= (count numbers) min-number-of-containers)))
                      count))

  )
