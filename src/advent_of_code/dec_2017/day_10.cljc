(ns advent-of-code.dec-2017.day-10
  (:require [ysera.test :refer [is=]]))

(def input (as-> "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24" $
                 (clojure.string/split $ #",")
                 (map read-string $)
                 (into [] $)))

(def test-input [3 4 1 5])

(def test-state {:numbers [0 1 2 3 4] :skip-size 0 :list-start-index 0})

(defn apply-length
  {:test (fn []
           (is= (apply-length test-state 3 5)
                {:numbers [3 4 2 1 0] :skip-size 1 :list-start-index 2})
           (is= (apply-length {:numbers [3 4 2 1 0] :skip-size 1 :list-start-index 2} 4 5)
                {:numbers [1 2 4 3 0] :skip-size 2 :list-start-index 2})
           (is= (apply-length {:numbers [1 2 4 3 0] :skip-size 2 :list-start-index 2} 1 5)
                {:numbers [3 0 1 2 4] :skip-size 3 :list-start-index 4})
           (is= (apply-length {:numbers [3 0 1 2 4] :skip-size 3 :list-start-index 4} 5 5)
                {:numbers [0 3 4 2 1] :skip-size 4 :list-start-index 1}))}
  [state length n]
  (let [[part-a part-b] (split-at length (:numbers state))
        numbers (cycle (concat (reverse part-a) part-b))
        list-move (+ length (:skip-size state))]
    {:numbers (->> numbers
                   (drop list-move)
                   (take n))
     :skip-size (inc (:skip-size state))
     :list-start-index (rem (+ (:list-start-index state) (- n list-move)) n)}))

(defn apply-lengths
  {:test (fn []
           (is= (apply-lengths test-state test-input)
                {:numbers [0 3 4 2 1] :skip-size 4 :list-start-index 1}))}
  [state lengths]
  (let [n (count (:numbers state))]
    (reduce (fn [state length] (apply-length state length n))
            state
            lengths)))

(defn solve-puzzle-a
  {:test (fn []
           (is= (solve-puzzle-a {:numbers [0 3 4 2 1] :skip-size 4 :list-start-index 1})
                12))}
  [state]
  (->> (cycle (:numbers state))
       (drop (:list-start-index state))
       (take 2)
       (reduce *)))

(def state {:numbers (range 256) :skip-size 0 :list-start-index 0})

(comment
  (->> (apply-lengths state input)
       (solve-puzzle-a))

  )
