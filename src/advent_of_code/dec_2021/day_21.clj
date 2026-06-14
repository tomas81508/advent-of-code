(ns advent-of-code.dec-2021.day-21
  (:require [advent-of-code.test :refer [is=]]))

; Player 1 starting position: 2
; Player 2 starting position: 1

(defn create-state
  [position1 position2]
  {:player-1       {:position position1
                    :score    0}
   :player-2       {:position position2
                    :score    0}
   :next-die-value 1
   :player-in-turn :player-1})

(def other-player {:player-1 :player-2 :player-2 :player-1})

(defn play-a-turn
  {:test (fn []
           (is= (play-a-turn (create-state 4 8))
                {:player-1       {:position 10
                                  :score    10}
                 :player-2       {:position 8
                                  :score    0}
                 :next-die-value 4
                 :player-in-turn :player-2}))}
  [state]
  (let [player-in-turn (:player-in-turn state)
        rolls-sum (+ (* 3 (:next-die-value state)) 3)]
    (-> state
        (update player-in-turn (fn [player]
                                 (let [new-position (let [np (mod (+ (:position player) rolls-sum) 10)]
                                                      (if (zero? np) 10 np))]
                                   (-> player
                                       (assoc :position new-position)
                                       (update :score + new-position)))))
        (update :next-die-value + 3)
        (update :player-in-turn other-player))))

(defn winner
  {:test (fn []
           (is= (winner {:player-1 {:score 40}
                         :player-2 {:score 1005}})
                :player-2)
           (is= (winner {:player-1 {:score 40}
                         :player-2 {:score 105}})
                nil))}
  [state]
  (->> (select-keys state [:player-1 :player-2])
       (some (fn [[player-id {score :score}]] (when (>= score 1000) player-id)))))

(defn play-a-game
  {:test (fn []
           (is= (play-a-game (create-state 4 8))
                [{:player-1       {:position 10, :score 1000}
                  :player-2       {:position 3, :score 745}
                  :next-die-value 994
                  :player-in-turn :player-2}
                 :player-1]))}
  [state]
  (loop [state state]
    (if-let [winner-player-id (winner state)]
      [state winner-player-id]
      (recur (play-a-turn state)))))

(defn puzzle-a []
  (let [[state player-id] (play-a-game (create-state 2 1))
        losing-player-id (other-player player-id)]
    (* (dec (:next-die-value state)) (get-in state [losing-player-id :score]))))

(comment
  (time (puzzle-a))
  ; "Elapsed time: 1.304541 msecs"
  ; => 797160
  )

(def quantum-die-outcomes {3 1 4 3 5 6 6 7 7 6 8 3 9 1})

(declare calculate-wins)

(defn calculate-wins-raw
  {:test (fn []
           (is= (calculate-wins {:position 1 :score 18}) 27))}
  [game]
  (->> quantum-die-outcomes
       (reduce-kv (fn [a result occurancies]
                    (let [new-position (let [np (mod (+ (:position game) result) 10)]
                                         (if (zero? np) 10 np))
                          new-score (+ (:score game) new-position)]
                      (+ a (if (>= new-score 21)
                             occurancies
                             (* occurancies (calculate-wins game))))))
                  0)))

(def calculate-wins (memoize calculate-wins-raw))

(comment
  (calculate-wins {:position 2 :score 0})
  )


(defn f-raw
  [x]
  (println "Reading")
  (inc x))

(f-raw 10)

(def f (memoize f-raw))

(f 101)



