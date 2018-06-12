(ns advent-of-code.dec-2017.day-06
  (:require [ysera.test :refer [deftest is=]]
            [ysera.collections :refer [seq-contains?]]))

(def puzzle-input [10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6])

(defn get-largest-index
  {:test (fn []
           (is= (get-largest-index [0 2 7 0])
                [2 7])
           (is= (get-largest-index [2 4 1 2])
                [1 4])
           (is= (get-largest-index [3 1 2 3])
                [0 3]))}
  [banks]
  (reduce (fn [[best-index best-bank] [index bank]]
            (if (> bank best-bank)
              [index bank]
              [best-index best-bank]))
          (map-indexed (fn [index item] [index item]) banks)))


(defn redistribute
  {:test (fn []
           (is= (redistribute [0 2 7 0])
                [2 4 1 2])
           (is= (redistribute [2 4 1 2])
                [3 1 2 3])
           (is= (redistribute [3 1 2 3])
                [0 2 3 4])
           (is= (redistribute [0 2 3 4])
                [1 3 4 1])
           (is= (redistribute [1 3 4 1])
                [2 4 1 2]))}
  [banks]
  (let [[index bank] (get-largest-index banks)
        index-size (count banks)]
    (reduce (fn [banks step]
              (let [index (mod (+ index (inc step)) index-size)]
                (update banks index inc)))
            (assoc banks index 0)
            (range bank))))


(defn get-number-of-cycles-for-next-redistribution
  {:test (fn []
           (is= (get-number-of-cycles-for-next-redistribution [0 2 7 0])
                {:redistributions 5
                 :first {:banks [2 4 1 2]
                         :index 1}}))}
  [banks]
  (loop [state {:redistributions 0
                :current-value   banks
                :old-values      [{:index 0
                                   :banks banks}]}]
    (let [value (redistribute (:current-value state))]
      (if-let [old-value (->> (:old-values state)
                              (filter (fn [old-value]
                                        (= (:banks old-value) value)))
                              (first))]
        {:redistributions (inc (:redistributions state))
         :first old-value}
        (recur (-> state
                   (assoc :current-value value)
                   (update :old-values conj {:banks value :index (inc (:redistributions state))})
                   (update :redistributions inc)))))))

(deftest puzzle-a
         (is= (time (:redistributions (get-number-of-cycles-for-next-redistribution puzzle-input)))
              ; "Elapsed time: 14616.61325 msecs"
              14029))

(deftest puzzle-b
         (is= (time (let [result (get-number-of-cycles-for-next-redistribution puzzle-input)]
                      (- (:redistributions result) (get-in result [:first :index]))))
              ; "Elapsed time: 14616.61325 msecs"
              2765))