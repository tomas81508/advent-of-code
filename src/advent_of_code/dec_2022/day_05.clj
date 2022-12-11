(ns advent-of-code.dec-2022.day-05
  (:require [ysera.test :refer [is is-not is= deftest]]))

; Done by my ten-year-old son Emil

(def input-data (->> (slurp "src/advent_of_code/dec_2022/day_05_input.txt")
                     (clojure.string/split-lines)
                     (map (fn [l]
                            (->> (re-find #"move (\d+) from (\d) to (\d)" l)
                                 (drop 1)
                                 (map read-string))))))


(def person {:name "Lime"
             :age  9})

(get person :name)

(assoc person :name "Emil")
(update person :age
        (fn [x] (inc x)))

(-> person
    (assoc :name "lime")
    (update :age inc)
    (update :name reverse))


(def l (seq "NFDGH"))

(drop 1 l)

(conj l \X)














;    [V] [G]             [H]
;[Z] [H] [Z]         [T] [S]
;[P] [D] [F]         [B] [V] [Q]
;[B] [M] [V] [N]     [F] [D] [N]
;[Q] [Q] [D] [F]     [Z] [Z] [P] [M]
;[M] [Z] [R] [D] [Q] [V] [T] [F] [R]
;[D] [L] [H] [G] [F] [Q] [M] [G] [W]
;[N] [C] [Q] [H] [N] [D] [Q] [M] [B]
; 1   2   3   4   5   6   7   8   9

(def state {1 (seq "ZPBQMDN")
            2 (seq "VHDMQZLC")
            3 (seq "GZFVDRHQ")
            4 (seq "NFDGH")
            5 (seq "QFN")
            6 (seq "TBFZVQD")
            7 (seq "HSVDZTMQ")
            8 (seq "QNPFGM")
            9 (seq "MRWB")})

(defn get-top-crate
  {:test (fn []
           (is= (get-top-crate state 5)
                \Q))}
  [state n]
  (-> (get state n)
      (first)))

(defn get-top-n-crates
  {:test (fn []
           (is= (get-top-n-crates state 2 5)
                (list \Q \F)))}
  [state n m]
  (->> (get state m)
       (take n)))

(defn move-1
  {:test (fn []
           (is= (-> (move-1 state 5 4)
                    (get-top-crate 4))
                \Q)
           (is= (-> (move-1 state 5 4)
                    (get-top-crate 5))
                \F))}
  [state from to]
  (let [crate (get-top-crate state from)]
    (-> state
        (update from (fn [crates]
                       (drop 1 crates)))
        (update to (fn [crates]
                     (conj crates crate))))))

(defn move-n
  {:test (fn []
           (is= (-> (move-n state 2 5 4)
                    (get 4))
                (list \Q \F \N \F \D \G \H)))}
  [state n from to]
  (let [remembered-crates (get-top-n-crates state n from)]
    (-> state
        (update from (fn [crates]
                       (drop n crates)))
        (update to (fn [crates]
                     (concat remembered-crates crates))))))



(defn move
  {:test (fn []
           (is= (-> (move state 2 4 5)
                    (get 5))
                (list \F \N \Q \F \N)))}
  [state n from to]
  (reduce (fn [state _]
            (move-1 state from to))
          state
          (range n)))


(def end-positions
  (->> input-data
       (reduce (fn [state [n from to]]
                 (move state n from to))
               state)))

(reduce (fn [a v]
          (str a (get-top-crate end-positions v)))
        ""
        (range 1 10))

(def end-positions-2
  (->> input-data
       (reduce (fn [state [n from to]]
                 (move-n state n from to))
               state)))

(reduce (fn [a v]
          (str a (get-top-crate end-positions-2 v)))
        ""
        (range 1 10))


