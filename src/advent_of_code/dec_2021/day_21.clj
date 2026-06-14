(ns advent-of-code.dec-2021.day-21
  (:require [advent-of-code.test :refer [is=]]
            [clojure.core.match :refer [occurrences]]))

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

; 3: 111
; 4: 112,121,211
; 5: 221,212,122,311,131,113
; 6: 123,132,231,321,213,312,222
; 7: 223,232,322,133,331,313
; 8: 332,323,233
; 9: 333

(def outcome {3 1 4 3 5 6 6 7 7 6 8 3 9 1})

(defn play-a-quantum-turn
  {:test (fn []
           (is= (play-a-quantum-turn {:player-1 {:position 2 :score 100} :player-2 {} :player-in-turn :player-1})
                [{:game {:player-1 {:position 5 :score 105} :player-2 {} :player-in-turn :player-2} :occurrences 1}
                 {:game {:player-1 {:position 6 :score 106} :player-2 {} :player-in-turn :player-2} :occurrences 3}
                 {:game {:player-1 {:position 7 :score 107} :player-2 {} :player-in-turn :player-2} :occurrences 6}
                 {:game {:player-1 {:position 8 :score 108} :player-2 {} :player-in-turn :player-2} :occurrences 7}
                 {:game {:player-1 {:position 9 :score 109} :player-2 {} :player-in-turn :player-2} :occurrences 6}
                 {:game {:player-1 {:position 10 :score 110} :player-2 {} :player-in-turn :player-2} :occurrences 3}
                 {:game {:player-1 {:position 1 :score 101} :player-2 {} :player-in-turn :player-2} :occurrences 1}]))}
  [game]
  (let [player-in-turn (:player-in-turn game)
        new-player-in-turn (other-player player-in-turn)]
    (->> outcome
         (map (fn [[rolls-sum occurrences]]
                {:game        {:player-in-turn    new-player-in-turn
                               new-player-in-turn (get game new-player-in-turn)
                               player-in-turn     (let [player (get game player-in-turn)
                                                        new-position (let [np (mod (+ (:position player) rolls-sum) 10)]
                                                                       (if (zero? np) 10 np))]
                                                    {:position new-position
                                                     :score    (+ (:score player) new-position)})}
                 :occurrences occurrences})))))

(def play-a-quantum-turn (memoize play-a-quantum-turn))

(defn play
  [game]
  (let [new-universes (play-a-quantum-turn game)
        wins (->> new-universes
                  (map (fn [{game :game occurrences :occurrences}]
                         (cond (>= (get-in game [:player-1 :score]) 21) [occurrences 0]
                               (>= (get-in game [:player-2 :score]) 21) [0 occurrences]
                               :else (let [[win-1 win-2] (play game)]
                                       [(* occurrences win-1) (* occurrences win-2)])))))]
    (->> wins
         (reduce (fn [a v] (map + a v))
                 [0 0]))))

(def play (memoize play))

(comment
  (play {:player-1 {:position 4 :score 0} :player-2 {:position 8 :score 0} :player-in-turn :player-1})
  ; 27464148626406
  )