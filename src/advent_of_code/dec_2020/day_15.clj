(ns advent-of-code.dec-2020.day-15
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(def puzzle-input [19 0 5 1 10 13])

(def test-input [0 3 6])

(defn next-number-to-say-aloud
  {:test (fn []
           (is= (next-number-to-say-aloud (list 6 3 0)) 0)
           (is= (next-number-to-say-aloud (list 0 6 3 0)) 3)
           (is= (next-number-to-say-aloud (list 3 0 6 3 0)) 3)
           (is= (next-number-to-say-aloud (list 3 3 0 6 3 0)) 1)
           (is= (next-number-to-say-aloud (list 1 3 3 0 6 3 0)) 0)
           (is= (next-number-to-say-aloud (list 0 1 3 3 0 6 3 0)) 4)
           (is= (next-number-to-say-aloud (list 4 0 1 3 3 0 6 3 0)) 0))}
  [history]
  (let [[n & r] history]
    (loop [i 0
           [f & r] r]
      (cond (nil? f) 0
            (= n f) (inc i)
            :else (recur (inc i) r)))))

(defn get-number-at
  {:test (fn []
           (is= (get-number-at (reverse test-input) 4) 0)
           (is= (get-number-at (reverse test-input) 9) 4)
           (is= (get-number-at (reverse test-input) 10) 0)
           (is= (get-number-at (reverse test-input) 2020) 436))}
  [history n]
  (loop [history history
         i (count history)]
    (if (< i n)
      (recur (conj history (next-number-to-say-aloud history))
             (inc i))
      (first history))))

(deftest puzzle-a
  (is= (time (get-number-at (reverse puzzle-input) 2020))
       ; "Elapsed time: 35.051243 msecs"
       1015))

(defn create-state
  {:test (fn []
           (is= (create-state [0 3 6])
                {:current-number 6
                 :current-index  3
                 :numbers        {0 1
                                  3 2}}))}
  [numbers]
  (as-> numbers $
        (map-indexed (fn [index n] [index n]) $)
        (drop-last $)
        (reduce (fn [a [index n]]
                  (assoc-in a [:numbers n] (inc index)))
                {:numbers {}}
                $)
        (assoc $ :current-number (last numbers)
                 :current-index (count numbers))))

(defn add-next-number-to-say-aloud-v2
  {:test (fn []
           (is= (add-next-number-to-say-aloud-v2 (create-state [0 3 6]))
                (create-state [0 3 6 0]))
           (is= (add-next-number-to-say-aloud-v2 (create-state [0 3 6 0]))
                (create-state [0 3 6 0 3]))
           (is= (add-next-number-to-say-aloud-v2 (create-state [0 3 6 0 3]))
                (create-state [0 3 6 0 3 3]))
           (is= (add-next-number-to-say-aloud-v2 (create-state [0 3 6 0 3 3]))
                (create-state [0 3 6 0 3 3 1]))
           (is= (add-next-number-to-say-aloud-v2 (create-state [0 3 6 0 3 3 1]))
                (create-state [0 3 6 0 3 3 1 0]))
           (is= (add-next-number-to-say-aloud-v2 (create-state [0 3 6 0 3 3 1 0]))
                (create-state [0 3 6 0 3 3 1 0 4]))
           (is= (add-next-number-to-say-aloud-v2 (create-state [0 3 6 0 3 3 1 0 4]))
                (create-state [0 3 6 0 3 3 1 0 4 0])))}
  [{current-number :current-number current-index :current-index numbers :numbers}]
  (let [index (get numbers current-number)
        next-index (inc current-index)]
    (if (nil? index)
      {:current-number 0
       :current-index  next-index
       :numbers        (assoc numbers current-number current-index)}
      (let [n (- current-index index)]
        {:current-number n
         :current-index  next-index
         :numbers        (assoc numbers current-number current-index)}))))

(defn get-number-at-v2
  {:test (fn []
           (is= (get-number-at-v2 (create-state test-input) 4) 0)
           (is= (get-number-at-v2 (create-state test-input) 9) 4)
           (is= (get-number-at-v2 (create-state test-input) 10) 0)
           (is= (get-number-at-v2 (create-state test-input) 2020) 436))}
  [state n]
  (loop [state state]
    (if (< (:current-index state) n)
      (recur (add-next-number-to-say-aloud-v2 state))
      (:current-number state))))

(deftest puzzle-a-v2
  (is= (time (get-number-at-v2 (create-state puzzle-input) 2020))
       ;"Elapsed time: 1.348726 msecs"
       1015))

(deftest puzzle-b
  (is= (time (get-number-at-v2 (create-state puzzle-input) 30000000))
       ; "Elapsed time: 16646.658533 msecs"
       201))



