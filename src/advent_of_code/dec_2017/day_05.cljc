(ns advent-of-code.dec-2017.day-05
  (:require [ysera.test :refer [deftest is is-not is=]]
            [clojure.string :as string]
            [clojure.string :as string]))

(defn jump
  {:test (fn []
           (is= (jump {:current      0
                       :instructions [0 3 0 1 -3]})
                {:current      0
                 :instructions [1 3 0 1 -3]})

           (is= (jump {:current      0
                       :instructions [1 3 0 1 -3]})
                {:current      1
                 :instructions [2 3 0 1 -3]})

           (is= (jump {:current      1
                       :instructions [2 3 0 1 -3]})
                {:current      4
                 :instructions [2 4 0 1 -3]})
           (is= (jump {:current      4
                       :instructions [2 4 0 1 -3]})
                {:current      1
                 :instructions [2 4 0 1 -2]})

           (is= (jump {:current      1
                       :instructions [2 4 0 1 -2]})
                {:current      5
                 :instructions [2 5 0 1 -2]}))}
  [{current :current instructions :instructions :as state}]
  {:current      (+ current (get instructions current))
   :instructions (update instructions current inc)})

(defn outside?
  {:test (fn []
           (is (outside? {:current      5
                          :instructions [2 5 0 1 -2]}))
           (is-not (outside? {:current      1
                              :instructions [2 4 0 1 -2]})))}
  [{current :current instructions :instructions}]
  (not (and (< current (count instructions))
            (not (neg? current)))))

(deftest puzzle-a
         (is= (loop [steps 0
                     state {:current      0
                            :instructions (->> (slurp "src/advent_of_code/dec_2017/day_05_input.txt")
                                               (string/split-lines)
                                               (map read-string)
                                               (into []))}]
                (if (outside? state)
                  steps
                  (recur (inc steps)
                         (jump state))))
              376976))


;; Puzzle b


(defn strange-jump
  {:test (fn []
           (is= (strange-jump {:current      0
                               :instructions [0 3 0 1 -3]})
                {:current      0
                 :instructions [1 3 0 1 -3]})

           (is= (strange-jump {:current      0
                               :instructions [1 3 0 1 -3]})
                {:current      1
                 :instructions [2 3 0 1 -3]})

           (is= (strange-jump {:current      1
                               :instructions [2 3 0 1 -3]})
                {:current      4
                 :instructions [2 2 0 1 -3]})
           (is= (strange-jump {:current      4
                               :instructions [2 2 0 1 -3]})
                {:current      1
                 :instructions [2 2 0 1 -2]})

           (is= (strange-jump {:current      1
                               :instructions [2 2 0 1 -2]})
                {:current      3
                 :instructions [2 3 0 1 -2]}))}
  [{current :current instructions :instructions :as state}]
  (let [instruction (get instructions current)]
    {:current      (+ current instruction)
     :instructions (update instructions current (if (< instruction 3) inc dec))}))


(deftest puzzle-a
         (is= (loop [steps 0
                     state {:current      0
                            :instructions (->> (slurp "src/advent_of_code/dec_2017/day_05_input.txt")
                                               (string/split-lines)
                                               (map read-string)
                                               (into []))}]
                (if (outside? state)
                  steps
                  (recur (inc steps)
                         (strange-jump state))))
              29227751))














