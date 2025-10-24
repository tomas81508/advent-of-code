(ns advent-of-code.dec-2018.day-17
  (:require [clojure.string :refer [split-lines join]]
            [advent-of-code.test :refer [is= is is-not]]
            [clojure.edn :as edn])
  (:import [clojure.lang PersistentQueue]))

(def input (-> (slurp "src/advent_of_code/dec_2018/day_17.txt")
               (split-lines)))

(def test-input ["x=495, y=2..7"
                 "y=7, x=495..501"
                 "x=501, y=3..7"
                 "x=498, y=2..4"
                 "x=506, y=1..2"
                 "x=498, y=10..13"
                 "x=504, y=10..13"
                 "y=13, x=498..504"])

(defn create-state
  {:test (fn []
           (let [state (create-state test-input)]
             (is= (get state [495 2]) \#)
             (is= (get state [501 5]) \#)
             (is= (get state [499 5]) nil)
             (is= (:max state) 13)))}
  [input]
  (let [state (reduce (fn [result line]
                        (if (= (first line) \x)
                          (let [[x y1 y2] (->> (re-seq #"\d+" line)
                                               (map edn/read-string))]
                            (reduce (fn [result y] (assoc result [x y] \#))
                                    result
                                    (range y1 (inc y2))))
                          (let [[y x1 x2] (->> (re-seq #"\d+" line)
                                               (map edn/read-string))]
                            (reduce (fn [result x] (assoc result [x y] \#))
                                    result
                                    (range x1 (inc x2))))))
                      {}
                      input)
        max-y (->> (keys state)
                   (map second)
                   (apply max))]
    (assoc state :max max-y)))

(def test-state (create-state test-input))
(def state (create-state input))

(defn sand? [state position] (nil? (get state position)))
;(defn clay? [state position] (= (get state position) \#))
(defn water? [state position] (= (get state position) \~))
(defn pouring-water? [state position] (= (get state position) \|))

(defn up [position] (map + position [0 -1]))
(defn down [position] (map + position [0 1]))
(defn right [position] (map + position [1 0]))
(defn left [position] (map + position [-1 0]))

(defn to-string
  [state]
  (let [x-values (->> (keys (dissoc state :max))
                      (map first))
        min-x (reduce min x-values)
        max-x (reduce max x-values)
        max-y (:max state)]
    (->> (for [y (range (inc max-y))
               x (range min-x (inc max-x))]
           [x y])
         (partition (inc (- max-x min-x)))
         (map (fn [row]
                (->> row
                     (map (fn [p] (str (get state p \.))))
                     (apply str)))))))

(comment
  (to-string test-state)
  )

(defn log-state
  [state]
  (println (join "\n" (to-string state))))

(defn fill-level-in-direction
  {:test (fn []
           (let [{water-positions :water-positions} (fill-level-in-direction test-state [500 6] [-1 0])]
             (is= water-positions [[500 6] [499 6] [498 6] [497 6] [496 6]])))}
  [state position direction]
  (loop [position position
         water-positions []]
    (if (or (sand? state position) (pouring-water? state position))
      (if (or (sand? state (down position))
              (pouring-water? state (down position)))
        {:pouring-down    position
         :water-positions water-positions}
        (recur (map + position direction) (conj water-positions position)))
      {:water-positions water-positions})))

(defn fill-level
  {:test (fn []
           (let [{state :state} (fill-level test-state [500 6])]
             (is (water? state [500 6]))
             (is (water? state [499 6]))
             (is (water? state [496 6]))
             (is-not (water? state [495 6]))))}
  [state position]
  {:pre [position]}
  (let [{pdl :pouring-down wpl :water-positions} (fill-level-in-direction state position [-1 0])
        {pdr :pouring-down wpr :water-positions} (fill-level-in-direction state position [1 0])
        water-positions (concat wpl wpr)
        pouring-down (->> [pdl pdr] (remove nil?))]
    {:state        (reduce (fn [state wp] (assoc state wp (if (empty? pouring-down) \~ \|)))
                           state
                           water-positions)
     :pouring-down pouring-down}))

(defn pour-down
  {:test (fn []
           (let [{state :state pouring-positions :pouring-positions} (pour-down test-state [500 0])]
             (is (pouring-water? state [500 1]))
             (is= pouring-positions (list [500 6] [500 5] [500 4] [500 3] [500 2] [500 1] [500 0])))
           (let [{end :end} (pour-down test-state [10 0])]
             (is= end true)))}
  [state position]
  (loop [state state
         position position
         positions (list)]
    (cond (> (second position) (:max state))
          {:state state :end true}

          (sand? state position)
          (recur (assoc state position \|)
                 (down position)
                 (conj positions position))

          :else
          {:state state :pouring-positions (if (pouring-water? state position)
                                             (conj positions position)
                                             positions)})))

(defn pour-water
  ; 1. down to the bottom with | remembering positions
  ; 3. fill and move up
  ; 4. until it spills over then forget the remembered positions and start over
  {:test (fn []
           (let [state (pour-water test-state [500 0])]
             (is (pouring-water? state [500 1]))
             (is (water? state [500 5]))
             (is (pouring-water? state [502 5]))
             (is (water? state [500 12]))))}
  [state position]
  (loop [state state
         queue (conj PersistentQueue/EMPTY position)]
    (let [position (peek queue)
          queue (pop queue)]
      (if-not position
        state
        (if (or (water? state position) (pouring-water? state position))
          (recur state queue)
          ; 1. down to the bottom with | remembering positions
          (let [{state :state pouring-positions :pouring-positions end :end} (pour-down state position)]
            (if end
              (recur state queue)
              (let [{state :state pds :pouring-down}
                    (loop [state state
                           pouring-position (first pouring-positions)]
                      (let [; 3. fill and move up
                            {state :state pds :pouring-down} (fill-level state pouring-position)]
                        (if (empty? pds)
                          (recur state (up pouring-position))
                          {:state state :pouring-down pds})))]
                (recur state (apply conj queue pds))))))))))


(comment
  (to-string (pour-water test-state [500 0]))
  (to-string state)
  )

(defn puzzle-1
  {:test (fn []
           (is= (puzzle-1 test-state)
                57))}
  [state]
  (let [water #{\~ \|}
        state (pour-water state [500 0])
        clay (->> (dissoc state :max)
                  (seq)
                  (filter (fn [[_ c]] (= c \#)))
                  (map first))
        min-y (->> (map second clay)
                   (reduce min))]
    (->> state
         (seq)
         (filter (fn [[_ v]] (contains? water v)))
         (filter (fn [[[_ y] _]] (>= y min-y)))
         (count))))

(defn puzzle-2
  {:test (fn []
           (is= (puzzle-2 test-state)
                29))}
  [state]
  (let [water #{\~ \|}
        state (pour-water state [500 0])
        clay (->> (dissoc state :max)
                  (seq)
                  (filter (fn [[_ c]] (= c \#)))
                  (map first))
        min-y (->> (map second clay)
                   (reduce min))]
    (->> state
         (seq)
         (filter (fn [[_ v]] (= v \~)))
         (filter (fn [[[_ y] _]] (>= y min-y)))
         (count))))

(comment
  (puzzle-1 state)
  ; 40879

  (puzzle-2 state)
  ; 34693
  )