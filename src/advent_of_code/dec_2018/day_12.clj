(ns advent-of-code.dec-2018.day-12
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

(def test-initial-state {:pots        (seq "#..#.#..##......###...###")
                         :start-index 0})

(def test-birth #{[\. \. \. \# \#]
                  [\. \. \# \. \.]
                  [\. \# \. \. \.]
                  [\. \# \. \# \.]
                  [\. \# \. \# \#]
                  [\. \# \# \. \.]
                  [\. \# \# \# \#]
                  [\# \. \# \. \#]
                  [\# \. \# \# \#]
                  [\# \# \. \# \.]
                  [\# \# \. \# \#]
                  [\# \# \# \. \.]
                  [\# \# \# \. \#]
                  [\# \# \# \# \.]})

(def initial-state {:pots        (-> (slurp "src/advent_of_code/dec_2018/day_12.txt")
                                     (string/split-lines)
                                     (first)
                                     (seq))
                    :start-index 0})

(def birth (->> (slurp "src/advent_of_code/dec_2018/day_12.txt")
                (string/split-lines)
                (drop 2)
                (remove (fn [line] (string/ends-with? line ".")))
                (map (fn [line] (seq (subs line 0 5))))
                (reduce conj #{})))

(defn tick
  {:test (fn []
           (is= (tick test-initial-state test-birth)
                {:pots        (seq "#...#....#.....#..#..#..#")
                 :start-index 0})
           (is= (tick {:pots        (seq "#...#....#.....#..#..#..#")
                       :start-index 0}
                      test-birth)
                {:pots        (seq "##..##...##....#..#..#..##")
                 :start-index 0})
           (is= (tick {:pots        (seq "##..##...##....#..#..#..##")
                       :start-index 0}
                      test-birth)
                {:pots        (seq "#.#...#..#.#....#..#..#...#")
                 :start-index -1})
           (is= (tick {:pots        (seq "#.#...#..#.#....#..#..#...#")
                       :start-index -1}
                      test-birth)
                {:pots        (seq "#.#..#...#.#...#..#..##..##")
                 :start-index 0}))}
  [state birth]
  (let [pots (->> (concat [\. \. \. \.] (:pots state) [\. \. \. \.])
                  (partition 5 1)
                  (map (fn [x] (if (birth x) \# \.)))
                  (reverse)
                  (drop-while #{\.})
                  (reverse))
        start-cut-pots (drop-while #{\.} pots)]
    {:pots        start-cut-pots
     :start-index (+ (:start-index state)
                     (- (count pots)
                        (+ (count start-cut-pots) 2)))}))
(defn calculate-score
  [{pots :pots start-index :start-index}]
  (->> pots
       (vec)
       (reduce-kv (fn [a index pot]
                    (if (= pot \#)
                      (+ a index start-index)
                      a))
                  0)))


(defn puzzle-1
  {:test (fn []
           (is= (puzzle-1 test-initial-state test-birth) 325)
           (is= (puzzle-1 initial-state birth) 1787))}
  [initial-state birth]
  (let [state (->> (range 20)
                   (reduce (fn [state _]
                             (tick state birth))
                           initial-state))]
    (calculate-score state)))

(defn puzzle-2
  [initial-state birth]
  (let [n 200
        state (->> (range n)
                   (reduce (fn [state _]
                             (tick state birth))
                           initial-state))
        score (calculate-score state)
        number-of-pots (->> state
                            (:pots)
                            (remove #{\.})
                            (count))]
    (+ score (* (- 50000000000 n) number-of-pots))))

(comment
  (puzzle-2 initial-state birth)
  )