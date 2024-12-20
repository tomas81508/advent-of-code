(ns advent-of-code.dec-2017.day-17
  (:require [advent-of-code.test :refer [is=]]))

(def puzzle-input 366)

(def initial-state {:numbers [0]
                    :current 0
                    :counter 1})

(defn spinlock-step
  {:test (fn []
           (is= (spinlock-step initial-state
                               3)
                {:numbers [0 1] :current 1 :counter 2})
           (is= (spinlock-step {:numbers [0 1]
                                :current 1
                                :counter 2}
                               3)
                {:numbers [0 2 1] :current 1 :counter 3}))}
  [state step]
  (let [index (inc (rem (+ (:current state) step)
                        (count (:numbers state))))
        [before after] (split-at index (:numbers state))]
    {:numbers (concat before
                      [(:counter state)]
                      after)
     :current index
     :counter (inc (:counter state))}))

(comment
  (time (->> (reduce (fn [state _]
                       (spinlock-step state puzzle-input))
                     initial-state
                     (range 2018))
             (:numbers)
             (partition 2 1)
             (some (fn [[v1 v2]]
                     (when (= v1 2017) v2)))))

  ; part 2
  (time
    (->> (range 1 50000001)
         (reduce (fn [a v]
                   (let [new-index (rem (+ (:index a) puzzle-input) v)]
                     (if (zero? new-index)
                       {:index (inc new-index) :value v}
                       (assoc a :index (inc new-index)))))
                 {:index 0
                  :value 0})))
  )