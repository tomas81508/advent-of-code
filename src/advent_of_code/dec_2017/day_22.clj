(ns advent-of-code.dec-2017.day-22
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.string :refer [split-lines]]))

(def infected (->> (slurp "src/advent_of_code/dec_2017/day_22_input.txt")
                   (split-lines)
                   (map-indexed (fn [y row] [y row]))
                   (reduce (fn [a [y row]]
                             (->> row
                                  (map-indexed (fn [x c] [x c]))
                                  (reduce (fn [a [x c]]
                                            (if (= c \#)
                                              (conj a [x y])
                                              a))
                                          a)))
                           #{})))

(def state {:position         [12 12]
            :facing           [0 -1]
            :infected         infected
            :caused-infection 0})

(def turn-left {[0 -1] [-1 0], [-1 0] [0 1], [0 1] [1 0], [1 0] [0 -1]})
(def turn-right {[0 -1] [1 0], [1 0] [0 1], [0 1] [-1 0], [-1 0] [0 -1]})

(defn step
  {:test (fn []
           (is= (step {:position         [1 1]
                       :facing           [0 -1]
                       :infected         #{[0 1] [2 0]}
                       :caused-infection 0})
                {:position         [0 1]
                 :facing           [-1 0]
                 :infected         #{[0 1] [2 0] [1 1]}
                 :caused-infection 1}))}
  [state]
  (let [current-node-infected (contains? (:infected state) (:position state))
        new-facing (if current-node-infected (turn-right (:facing state)) (turn-left (:facing state)))]
    (-> state
        (assoc :facing new-facing)
        (update :position (fn [p] (map + p new-facing)))
        (update :infected (fn [infected]
                            (if current-node-infected
                              (disj infected (:position state))
                              (conj infected (:position state)))))
        (update :caused-infection (fn [x] (if current-node-infected x (inc x)))))))

(defn steps
  [state n]
  (reduce (fn [state _] (step state))
          state
          (range n)))

(comment
  (-> (steps {:position         [1 1]
              :facing           [0 -1]
              :infected         #{[0 1] [2 0]}
              :caused-infection 0}
             10000)
      (:caused-infection))

  (-> (steps state 10000)
      (:caused-infection)))


;Clean nodes become weakened.
;Weakened nodes become infected.
;Infected nodes become flagged.
;Flagged nodes become clean.

(def state-transition {:cleaned  :weakened
                       :weakened :infected
                       :infected :flagged
                       :flagged  :cleaned})

(defn step-2
  {:test (fn []
           (is= (step-2 {:position         [1 1]
                         :facing           [0 -1]
                         :infected         #{[0 1] [2 0]}
                         :weakened         #{}
                         :flagged          #{}
                         :caused-infection 0})
                {:position         [0 1]
                 :facing           [-1 0]
                 :infected         #{[0 1] [2 0]}
                 :weakened         #{[1 1]}
                 :flagged          #{}
                 :caused-infection 0}))}
  [{current-position :position
    facing           :facing
    :as              state}]
  (let [current-node-state (cond (contains? (:infected state) current-position) :infected
                                 (contains? (:weakened state) current-position) :weakened
                                 (contains? (:flagged state) current-position) :flagged
                                 :else :cleaned)
        new-state (state-transition current-node-state)
        new-facing (case current-node-state
                     :cleaned (turn-left facing)
                     :weakened facing
                     :infected (turn-right facing)
                     :flagged (map * [-1 -1] facing))]
    (as-> state $
          (assoc $ :facing new-facing)
          ; remove previous state
          (if (= current-node-state :cleaned)
            $
            (update $ current-node-state disj current-position))
          ; add new state
          (if (= new-state :cleaned)
            $
            (update $ new-state conj current-position))
          (update $ :position (fn [p] (map + p new-facing)))
          (if (= new-state :infected)
            (update $ :caused-infection inc)
            $))))

(defn steps-2
  [state n]
  (reduce (fn [state _] (step-2 state))
          state
          (range n)))

(def state-2 {:position         [12 12]
              :facing           [0 -1]
              :infected         infected
              :weakened         #{}
              :flagged          #{}
              :caused-infection 0})

(comment
  (time (-> (steps-2 {:position         [1 1]
                      :facing           [0 -1]
                      :infected         #{[0 1] [2 0]}
                      :weakened         #{}
                      :flagged          #{}
                      :caused-infection 0}
                     10000000)
            (:caused-infection)))

  (time (-> (steps-2 state-2 10000000)
            (:caused-infection)))
  )










