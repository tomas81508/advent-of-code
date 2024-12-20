(ns advent-of-code.dec-2018.day-09
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]))


(defn create-state
  {:test (fn []
           (is= (create-state 2)
                {:circle         {0 {:p 0 :n 0 :id 0}}
                 :circle-count   1
                 :current-marble 0
                 :marble-value   1
                 :current-player 0
                 :players        [0 0]
                 :player-count   2}))}
  [number-of-players]
  (let [players (into [] (repeat number-of-players 0))]
    {:circle         {0 {:p 0 :n 0 :id 0}}
     :circle-count   1
     :current-marble 0
     :marble-value   1
     :current-player 0
     :players        players
     :player-count   (count players)}))


(declare circle->string)


(defn get-marble
  [state id]
  (get-in state [:circle id]))

(declare add-marble)
(declare add-marbles)

(defn get-marble-id-in-direction
  {:test (fn []
           (is= (-> (create-state 9)
                    (add-marble)
                    (add-marble)
                    (add-marble)
                    (add-marble)
                    (get-marble-id-in-direction -2))
                3)
           (is= (-> (create-state 9)
                    (add-marble)
                    (add-marble)
                    (add-marble)
                    (add-marble)
                    (get-marble-id-in-direction 3))
                3))}
  [state n]
  {:pre [(number? n)]}
  (loop [id (:current-marble state)
         n n]
    (cond (zero? n)
          id

          (pos? n)
          (recur (:n (get-marble state id))
                 (dec n))

          :else
          (recur (:p (get-marble state id))
                 (inc n)))))


(defn remove-marble
  "Removes the marble with given id from the circle."
  {:test (fn []
           (is= (-> (create-state 9)
                    (add-marbles 4)
                    (remove-marble 2)
                    (circle->string))
                "4 | 0 (4) 1 3"))}
  [state id]
  (let [marble-to-remove (get-marble state id)
        left-marble-id (:p marble-to-remove)
        right-marble-id (:n marble-to-remove)]
    (-> state
        (assoc-in [:circle left-marble-id :n] right-marble-id)
        (assoc-in [:circle right-marble-id :p] left-marble-id)
        (update :circle (fn [circle]
                          (dissoc circle id)))
        (update :circle-count dec))))



(defn add-marble
  "Adds a marble to the circle."
  {:test (fn []
           (is= (-> (create-state 2)
                    (add-marble))
                {:circle         {0 {:p 1 :n 1 :id 0}
                                  1 {:p 0 :n 0 :id 1}}
                 :circle-count   2
                 :current-marble 1
                 :marble-value   2
                 :current-player 1
                 :players        [0 0]
                 :player-count   2})
           (is= (-> (create-state 2)
                    (add-marbles 22)
                    (circle->string))
                "0 | 0 16 8 17 4 18 9 19 2 20 10 21 5 (22) 11 1 12 6 13 3 14 7 15")
           (is= (-> (create-state 2)
                    (add-marbles 23)
                    (circle->string))
                "1 | 0 16 8 17 4 18 (19) 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15"))}
  [state]
  (-> (if (zero? (mod (:marble-value state) 23))
        (let [remove-marble-id (get-marble-id-in-direction state -7)]
          (-> state
              (update-in [:players (:current-player state)] (fn [x] (+ x (:marble-value state) remove-marble-id)))
              (assoc :current-marble (:n (get-marble state remove-marble-id)))
              (remove-marble remove-marble-id)))
        (let [left-marble-id (get-marble-id-in-direction state 1)
              right-marble-id (get-marble-id-in-direction state 2)
              new-marble {:id (:marble-value state)
                          :p  left-marble-id
                          :n  right-marble-id}]
          (-> state
              (assoc-in [:circle (:marble-value state)] new-marble)
              (assoc-in [:circle left-marble-id :n] (:id new-marble))
              (assoc-in [:circle right-marble-id :p] (:id new-marble))
              (update :circle-count inc)
              (assoc :current-marble (:id new-marble)))))
      (update :current-player (fn [index] (mod (inc index) (:player-count state))))
      (update :marble-value inc)))


(defn add-marbles
  {:test (fn []
           (is= (-> (create-state 9)
                    (add-marbles 4)
                    (circle->string))
                "4 | 0 (4) 2 1 3"))}
  [state n]
  (if (zero? n)
    state
    (recur (add-marble state) (dec n))))


(defn circle->string
  {:test (fn []
           (is= (-> (create-state 9)
                    (add-marble)
                    (add-marble)
                    (add-marble)
                    (add-marble)
                    (circle->string))
                "4 | 0 (4) 2 1 3"))}
  [state]
  (first (reduce (fn [[result id] _]
                   [(if (= id (:current-marble state))
                      (str result " (" id ")")
                      (str result " " id))
                    (get-in state [:circle id :n])])
                 [(str (:current-player state) " |") 0]
                 (range (count (keys (:circle state)))))))



(deftest example-tests

  ; Before: "Elapsed time: 242.463971 msecs"
  ; "Elapsed time: 13.343846 msecs"
  (time (is= (as-> (create-state 10) $
                   (add-marbles $ 1618)
                   (:players $)
                   (apply max $))
             8317))

  ; Before: "Elapsed time: 5672.388958 msecs"
  ; "Elapsed time: 41.211851 msecs"
  (time (is= (as-> (create-state 13) $
                   (add-marbles $ 7999)
                   (:players $)
                   (apply max $))
             146373))

  ; Before: "Elapsed time: 132.610799 msecs"
  ; "Elapsed time: 7.150494 msecs"
  (time (is= (as-> (create-state 17) $
                   (add-marbles $ 1104)
                   (:players $)
                   (apply max $))
             2764))

  ; Before: "Elapsed time: 4002.365897 msecs"
  ; "Elapsed time: 29.698666 msecs"
  (time (is= (as-> (create-state 21) $
                   (add-marbles $ 6111)
                   (:players $)
                   (apply max $))
             54718))

  ; Before: "Elapsed time: 3423.621054 msecs"
  ; "Elapsed time: 34.835079 msecs"
  (time (is= (as-> (create-state 30) $
                   (add-marbles $ 5807)
                   (:players $)

                   (apply max $))
             37305)))

(comment
  (deftest puzzle-part-1
    ; Before: "Elapsed time: 511725.233347 msecs"
    ; "Elapsed time: 270.461641 msecs"
    (time (is= (as-> (create-state 446) $
                     (add-marbles $ 71522)
                     (:players $)
                     (apply max $))
               390592)))

  (deftest puzzle-part-2
    ; Before: Will go on forever...
    ; "Elapsed time: 39383.874907 msecs"
    (time (is= (as-> (create-state 446) $
                     (add-marbles $ 7152200)
                     (:players $)
                     (apply max $))
               3277920293)))
  )



























