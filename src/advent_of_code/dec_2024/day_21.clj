(ns advent-of-code.dec-2024.day-21
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

(def input ["286A" "480A" "140A" "413A" "964A"])

(def test-input ["029A" "980A" "179A" "456A" "379A"])

(def directional-pad {[1 0] \^
                      [2 0] \A
                      [0 1] \<
                      [1 1] \v
                      [2 1] \>})

(def numerical-pad {[0 0] \7
                    [1 0] \8
                    [2 0] \9
                    [0 1] \4
                    [1 1] \5
                    [2 1] \6
                    [0 2] \1
                    [1 2] \2
                    [2 2] \3
                    [1 3] \0
                    [2 3] \A})

(defn get-position-of-character
  {:test (fn []
           (is= (get-position-of-character numerical-pad \1)
                [0 2]))}
  [pad character]
  (->> pad
       (some (fn [[k v]] (when (= v character) k)))))

(def directions [[0 1] [0 -1] [-1 0] [1 0]])

(defn get-finished-sequences
  [boundary end-pos]
  (->> boundary
       (keep (fn [[pos s]] (when (= pos end-pos) s)))))

(defn get-shortest-sequences
  {:test (fn []
           (is= (->> (get-shortest-sequences numerical-pad [1 2] [2 1])
                     (into #{}))
                #{[[0 -1] [1 0]] [[1 0] [0 -1]]}))}
  [pad start-pos end-pos]
  (loop [visited #{start-pos}
         boundary #{[start-pos []]}]
    (let [finished-sequences (get-finished-sequences boundary end-pos)]
      (if-not (empty? finished-sequences)
        finished-sequences
        (let [boundary (reduce (fn [boundary [pos current-seq]]
                                 (reduce (fn [boundary d]
                                           (let [new-pos (map + d pos)]
                                             (if (and (contains? pad new-pos)
                                                      (not (contains? visited new-pos)))
                                               (conj boundary [new-pos (conj current-seq d)])
                                               boundary)))
                                         boundary
                                         directions))
                               #{}
                               boundary)
              visited (->> boundary
                           (map first)
                           (reduce conj visited))]
          (recur visited boundary))))))

(def step->directional-pad-positions {[1 0]  [2 1]
                                      [-1 0] [0 1]
                                      [0 -1] [1 0]
                                      [0 1]  [1 1]
                                      \A     [2 0]})

(defn get-cost-of-sequence
  {:test (fn []
           (is= (get-cost-of-sequence [[0 1] [1 0]] 0)
                3)
           (is= (get-cost-of-sequence [] 0)
                1)
           (is= (get-cost-of-sequence [] 1)
                1)
           (is= (get-cost-of-sequence [] 2)
                1)
           (is= (get-cost-of-sequence [[1 0]] 1)
                ; right
                4)
           (is= (get-cost-of-sequence [[-1 0]] 1)
                ; left
                8)
           (is= (get-cost-of-sequence [[0 1]] 1)
                ; down
                6)
           (is= (get-cost-of-sequence [[0 -1]] 1)
                ; up
                4)
           (is= (get-cost-of-sequence [[0 -1]] 2)
                ; up
                12)
           (is= (get-cost-of-sequence [[0 1]] 2)
                ; down
                16)
           (is= (get-cost-of-sequence [[-1 0]] 2)
                ; left
                18)
           (is= (get-cost-of-sequence [[1 0]] 2)
                ; right
                10)
           (is= (get-cost-of-sequence [[0 1] [-1 0] [-1 0]] 1)
                10))}
  [s remaining-robots]
  (if (zero? remaining-robots)
    (inc (count s))
    (->> (conj s \A)
         (cons \A)
         (map step->directional-pad-positions)
         (partition 2 1)
         (map (fn [[start-pos end-pos]]
                (->> (get-shortest-sequences directional-pad start-pos end-pos)
                     (map (fn [s]
                            (get-cost-of-sequence s (dec remaining-robots))))
                     (apply min))))
         (reduce +))))

(def get-cost-of-sequence (memoize get-cost-of-sequence))

(defn get-shortest-sequences-total
  {:test (fn []
           (is= (get-shortest-sequences-total [2 3] [1 3] 2)
                18))}
  [start-pos end-pos number-of-robots]
  (let [shortest-sequences-numerical-pad (get-shortest-sequences numerical-pad start-pos end-pos)]
    (apply min
           (map (fn [s]
                  (get-cost-of-sequence s number-of-robots))
                shortest-sequences-numerical-pad))))

(defn get-shortest-sequence-for-code
  {:test (fn []
           (is= (get-shortest-sequence-for-code "029A" 2)
                68))}
  [code number-of-robots]
  (let [the-sequence (cons \A (seq code))
        positions (map (fn [c]
                         (get-position-of-character numerical-pad c))
                       the-sequence)]
    (->> (partition 2 1 positions)
         (map (fn [[start-pos end-pos]]
                (get-shortest-sequences-total start-pos end-pos number-of-robots)))
         (reduce +))))

(defn get-code-numerical-value
  {:test (fn []
           (is= (get-code-numerical-value "029A")
                29))}
  [code]
  (->> code
       (re-find #"\d+")
       (str "10r")
       (read-string)))

(read-string "10r029")

(defn get-code-complexity
  {:test (fn []
           (is= (get-code-complexity "029A" 2) 1972))}
  [code number-of-robots]
  (* (get-code-numerical-value code)
     (get-shortest-sequence-for-code code number-of-robots)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 126384))}
  [input]
  (->> input
       (map (fn [code]
              (get-code-complexity code 2)))
       (reduce +)))

(defn part-2
  [input]
  (->> input
       (map (fn [code]
              (get-code-complexity code 25)))
       (reduce +)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 7.4255 msecs"
  ;=> 184718

  (time (part-2 input))
  ;;
  )