(ns advent-of-code.dec-2023.day-03
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is= is-not]]))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_03_input.txt")
                (clojure.string/split-lines)))

(def test-input ["467..114.."
                 "...*......"
                 "..35..633."
                 "......#..."
                 "617*......"
                 ".....+.58."
                 "..592....."
                 "......755."
                 "...$.*...."
                 ".664.598.."])

(def digit-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn get-state
  {:test (fn []
           (is= (get-state test-input)
                {:numbers {[0 0] "467"
                           [5 0] "114"
                           [2 2] "35"
                           [6 2] "633"
                           [0 4] "617"
                           [7 5] "58"
                           [2 6] "592"
                           [6 7] "755"
                           [1 9] "664"
                           [5 9] "598"}
                 :symbols {\* #{[3 4] [5 8] [3 1]}
                           \# #{[6 3]}
                           \+ #{[5 5]}
                           \$ #{[3 8]}}}))}
  [input]
  (let [max-y (count input)
        max-x (count (first input))]
    (-> (reduce (fn [a y]
                  (reduce (fn [a x]
                            (let [value (get-in input [y x])]
                              (cond (contains? digit-chars value)
                                    (-> (if-not (get-in a [:current :position])
                                          (assoc-in a [:current :position] [x y])
                                          a)
                                        (update-in [:current :number] str value))

                                    (= value \.)
                                    (if-let [n (get-in a [:current :number])]
                                      (as-> a $
                                            (assoc-in $ [:result :numbers (get-in $ [:current :position])] n)
                                            (assoc $ :current {:number nil :position nil}))
                                      a)

                                    :else

                                    (-> (if-let [n (get-in a [:current :number])]
                                          (as-> a $
                                                (assoc-in $ [:result :numbers (get-in $ [:current :position])] n)
                                                (assoc $ :current {:number nil :position nil}))
                                          a)
                                        (update-in [:result :symbols value] (fn [ps]
                                                                              (if-not ps
                                                                                #{[x y]}
                                                                                (conj ps [x y]))))))))
                          a
                          (range max-x)))
                {:current {:number   nil
                           :position nil}
                 :result  {:numbers {}
                           :symbols {}}}
                (range max-y))
        (:result))))

(def test-state (get-state test-input))

(def state (get-state input))

(def directions #{[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]})

(defn get-neighbors
  {:test (fn []
           (is= (get-neighbors {:numbers {[0 0] "467" [0 4] "617"}} [0 4])
                #{[-1 3] [-1 4] [-1 5] [0 3] [0 5] [1 3] [1 5] [2 3] [2 5] [3 3] [3 4] [3 5]}))}
  [state position]
  (let [number (get-in state [:numbers position])
        number-length (count number)
        number-positions (->> (range number-length)
                              (map (fn [l] (map + position [l 0])))
                              (into #{}))]
    (as-> number-positions $
          (reduce (fn [a p]
                    (->> directions
                         (map (fn [d] (map + d p)))
                         (reduce conj a)))
                  #{}
                  $)
          (into #{} $)
          (clojure.set/difference $ number-positions))))

(defn next-to-symbol?
  [state position]
  (let [neighbors (get-neighbors state position)
        symbol-positions (->> (:symbols state)
                              (vals)
                              (apply clojure.set/union))]
    (not (empty? (clojure.set/intersection symbol-positions neighbors)))))

(deftest puzzle-a
  (is= (time (let [state (get-state input)]
               (->> (:numbers state)
                    (reduce-kv (fn [a p n]
                                 (if (next-to-symbol? state p)
                                   (+ a (read-string n))
                                   a))
                               0))))
       ; "Elapsed time: 165.308896 msecs"
       533784))

; part 2

(defn get-state2
  {:test (fn []
           (is= (get-state2 test-state)
                {:numbers {[0 0] "467" [1 0] "467" [2 0] "467"
                           [5 0] "114" [6 0] "114" [7 0] "114"
                           [2 2] "35" [3 2] "35"
                           [6 2] "633" [7 2] "633" [8 2] "633"
                           [0 4] "617" [1 4] "617" [2 4] "617"
                           [7 5] "58" [8 5] "58"
                           [2 6] "592" [3 6] "592" [4 6] "592"
                           [6 7] "755" [7 7] "755" [8 7] "755"
                           [1 9] "664" [2 9] "664" [3 9] "664"
                           [5 9] "598" [6 9] "598" [7 9] "598"}
                 :symbols {\* #{[3 4] [5 8] [3 1]}
                           \# #{[6 3]}
                           \+ #{[5 5]}
                           \$ #{[3 8]}}}))}
  [state]
  (update state :numbers
          (fn [numbers]
            (reduce-kv (fn [a k v]
                         (let [l (count v)]
                           (reduce (fn [a i]
                                     (assoc a (map + k [i 0]) v))
                                   a
                                   (range l))))
                       {}
                       numbers))))

(def test-state2 (get-state2 test-state))

(def state2 (get-state2 state))

(defn exactly-two-adjacent-numbers
  {:test (fn []
           (is-not (exactly-two-adjacent-numbers test-state2 [3 4]))
           (is= (exactly-two-adjacent-numbers test-state2 [5 8])
                #{598 755})
           (is= (exactly-two-adjacent-numbers test-state2 [3 1])
                #{467 35}))}
  [state2 position]
  (let [adjacent-numbers (->> directions
                              (map (fn [d] (map + d position)))
                              (map (fn [p] (get-in state2 [:numbers p])))
                              (remove nil?)
                              (map read-string)
                              (into #{}))]
    (when (= (count adjacent-numbers) 2)
      adjacent-numbers)))

(deftest puzzle-b
  (is= (time (let [stars (get-in state2 [:symbols \*])]
               (reduce (fn [a v]
                         (if-let [adjacent-numbers (exactly-two-adjacent-numbers state2 v)]
                           (+ a (apply * adjacent-numbers))
                           a))
                       0
                       stars)))
       ; "Elapsed time: 6.175152 msecs"
       78826761))






