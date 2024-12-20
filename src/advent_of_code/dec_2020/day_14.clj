(ns advent-of-code.dec-2020.day-14
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_14.txt")
       (clojure.string/split-lines)))

(def test-input ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                 "mem[8] = 11"
                 "mem[7] = 101"
                 "mem[8] = 0"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:program [{:mask   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                            :writes [{:memory 8 :value 11}
                                     {:memory 7 :value 101}
                                     {:memory 8 :value 0}]}]
                 :memory  {}}))}
  [input]
  {:program (let [mem-pattern (re-pattern "mem\\[([\\d]+)\\] = ([\\d]+)")]
              (as-> (reduce (fn [[a current] v]
                              (if (clojure.string/starts-with? v "mask = ")
                                [(if current (conj a current) a)
                                 {:mask   (subs v 7)
                                  :writes []}]
                                (let [[_ mem value] (re-matches mem-pattern v)]
                                  [a (update current :writes conj
                                             {:memory (read-string mem)
                                              :value  (read-string value)})])))
                            [[] nil]
                            input) $
                    (conj (first $) (second $))))
   :memory  {}})

(defn get-memory-value
  {:test (fn []
           (is= (get-memory-value "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 11)
                73)
           (is= (get-memory-value "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 101)
                101)
           (is= (get-memory-value "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 0)
                64))}
  [mask value]
  (let [binary-value (Integer/toBinaryString value)
        reversed-mask (reverse mask)
        reversed-value (concat (reverse binary-value) (repeat 36 \0))]
    (-> (map (fn [x y]
               (if (= x \X) y x))
             reversed-mask
             reversed-value)
        (reverse)
        (clojure.string/join)
        (Long/parseLong 2))))

(defn run-program
  {:test (fn []
           (is= (-> (create-state test-input)
                    (run-program))
                {7 101
                 8 64}))}
  [{program :program memory :memory}]
  (reduce (fn [memory {mask :mask writes :writes}]
            (reduce (fn [memory {m :memory value :value}]
                      (assoc memory m (get-memory-value mask value)))
                    memory
                    writes))
          memory
          program))

(deftest puzzle-a
  (is= (time (->> (get-puzzle-input)
                  (create-state)
                  (run-program)
                  (vals)
                  (reduce +)))
       ; "Elapsed time: 11.817048 msecs"
       7440382076205))

(def test-input-2 ["mask = 000000000000000000000000000000X1001X"
                   "mem[42] = 100"
                   "mask = 00000000000000000000000000000000X0XX"
                   "mem[26] = 1"])

(defn apply-mask
  {:test (fn []
           (is= (apply-mask "000000000000000000000000000000X1001X"
                            42)
                "000000000000000000000000000000X1101X")
           (is= (apply-mask "00000000000000000000000000000000X0XX"
                            26)
                "00000000000000000000000000000001X0XX"))}
  [mask memory]
  (let [binary-value (Integer/toBinaryString memory)
        reversed-mask (reverse mask)
        reversed-value (into [] (reverse binary-value))]
    (->> (reduce (fn [after-mask [index mask-bit]]
                   (conj after-mask
                         (if (= mask-bit \0)
                           (or (get reversed-value index) \0)
                           mask-bit)))
                 (list)
                 (map-indexed (fn [index item] [index item])
                              reversed-mask))
         (clojure.string/join))))

(defn exp
  {:test (fn []
           (is= (exp 2 0) 1)
           (is= (exp 2 3) 8))}
  [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
                  (recur (* x acc) (dec n)))))

(defn get-memories
  {:test (fn []
           (is= (get-memories "000000000000000000000000000000X1001X"
                              42)
                #{26 27 58 59})
           (is= (get-memories "00000000000000000000000000000000X0XX"
                              26)
                #{16 17 18 19 24 25 26 27}))}
  [mask memory]
  (let [after-mask-string (apply-mask mask memory)]
    (->> (reverse after-mask-string)
         (map-indexed (fn [index value] [index value]))
         (reduce (fn [a [index value]]
                   (condp = value
                     \0 a
                     \1 (map (fn [x] (+ x (exp 2 index)))
                             a)
                     \X (reduce (fn [a x]
                                  (conj a (+ x (exp 2 index))))
                                a
                                a)))
                 #{0})
         (set))))

(defn run-program-version-2
  {:test (fn []
           (is= (-> (create-state test-input-2)
                    (run-program-version-2))
                {58 100 59 100
                 16 1 17 1 18 1 19 1 24 1 25 1 26 1 27 1}))}
  [{program :program memory :memory}]
  (reduce (fn [memory {mask :mask writes :writes}]
            (reduce (fn [memory {m :memory value :value}]
                      (reduce (fn [memory m]
                                (assoc memory m value))
                              memory
                              (get-memories mask m)))
                    memory
                    writes))
          memory
          program))

(defn sum-values
  {:test (fn []
           (is= (-> (create-state test-input-2)
                    (run-program-version-2)
                    (sum-values))
                208))}
  [memory]
  (apply + (vals memory)))

(deftest puzzle-b
  (is= (time (-> (create-state (get-puzzle-input))
                 (run-program-version-2)
                 (sum-values)))
       ; "Elapsed time: 232.403889 msecs"
       4200656704538))












