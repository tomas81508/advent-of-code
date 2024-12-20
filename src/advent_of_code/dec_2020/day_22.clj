(ns advent-of-code.dec-2020.day-22
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(def state {:player1 [31 33 27 43 29 25 36 11 15 5 14 34 7 18 26 41 19 45 12 1 8 35 44 30 50]
            :player2 [42 40 6 17 3 16 22 23 32 21 24 46 49 48 38 47 13 9 39 20 10 2 37 28 4]})

(def test-state {:player1 [9 2 6 3 1]
                 :player2 [5 8 4 7 10]})

(defn play-a-round
  {:test (fn []
           (is= (play-a-round test-state)
                {:player1 [2 6 3 1 9 5]
                 :player2 [8 4 7 10]}))}
  [state]
  (let [[c1 & r1] (:player1 state)
        [c2 & r2] (:player2 state)]
    (if (> c1 c2)
      {:player1 (concat r1 [c1 c2])
       :player2 (or r2 [])}
      {:player1 (or r1 [])
       :player2 (concat r2 [c2 c1])})))

(defn get-winner
  {:test (fn []
           (is= (get-winner {:player1 [2 6 3 1 9 5 8 4 7 10]
                             :player2 []})
                :player1))}
  [state]
  (->> (seq state)
       (some (fn [[player cards]] (when (empty? cards)
                                    (if (= player :player1) :player2 :player1))))))

(defn play-to-the-end
  {:test (fn []
           (is= (play-to-the-end test-state)
                {:player1 []
                 :player2 [3 2 10 6 8 5 9 4 7 1]}))}
  [state]
  (if (get-winner state)
    state
    (recur (play-a-round state))))

(defn score
  {:test (fn []
           (is= (score {:player1 []
                        :player2 [3 2 10 6 8 5 9 4 7 1]})
                306))}
  [state]
  (some (fn [[_ cards]] (when-not (empty? cards)
                          (->> (reverse cards)
                               (map-indexed (fn [index card] (* (inc index) card)))
                               (apply +))))
        (seq state)))

(deftest puzzle-a
  (is= (time (-> state
                 (play-to-the-end)
                 (score)))
       ; "Elapsed time: 2.068916 msecs"
       32413))


;(defn play-a-round-2
;  {:test (fn []
;           (is= (play-a-round-2 test-state)
;                {:player1 [2 6 3 1 9 5]
;                 :player2 [8 4 7 10]})
;           (is= (play-a-round-2 {:player1 [4 9 8 5 2]
;                                 :player2 [3 10 1 7 6]})
;                {:player1 [9 8 5 2]
;                 :player2 [10 1 7 6 3 4]}))}
;  [state]
;  (let [[c1 & r1] (:player1 state)
;        [c2 & r2] (:player2 state)
;        sub-game-winner (and (<= c1 (count r1))
;                             (<= c2 (count r2))
;                             (-> (play-to-the-end-2 {:player1 r1 :player2 r2})
;                                 (get-winner)))]
;    (if (or (= sub-game-winner :player1)
;            (and (not sub-game-winner)
;                 (> c1 c2)))
;      {:player1 (concat r1 [c1 c2])
;       :player2 (or r2 [])}
;      {:player1 (or r1 [])
;       :player2 (concat r2 [c2 c1])})))

(defn play-to-the-end-2
  {:test (fn []
           (is= (play-to-the-end-2 {:player1 [43 19]
                                    :player2 [2 29 14]})
                {:player1 [:winner]
                 :player2 []})
           (is= (play-to-the-end-2 test-state)
                {:player1 []
                 :player2 [7 5 6 2 4 1 10 8 9 3]}))}
  [state]
  (loop [state state
         history #{}]
    (cond (get-winner state)
          state

          (contains? history state)
          {:player1 [:winner] :player2 []}

          :else
          (let [[c1 & r1] (:player1 state)
                [c2 & r2] (:player2 state)
                sub-game-winner (and (<= c1 (count r1))
                                     (<= c2 (count r2))
                                     (-> (play-to-the-end-2 {:player1 (take c1 r1) :player2 (take c2 r2)})
                                         (get-winner)))
                next-state (if (or (= sub-game-winner :player1)
                                   (and (not sub-game-winner)
                                        (> c1 c2)))
                             {:player1 (concat r1 [c1 c2])
                              :player2 (or r2 [])}
                             {:player1 (or r1 [])
                              :player2 (concat r2 [c2 c1])})]
            (recur next-state (conj history state))))))

(deftest puzzle-b
  (is= (time (-> state
                 (play-to-the-end-2)
                 (score)))
       ; "Elapsed time: 3468.471383 msecs"
       31596))
