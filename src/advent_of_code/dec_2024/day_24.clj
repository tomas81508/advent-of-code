(ns advent-of-code.dec-2024.day-24
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def input-1 (->> (slurp "src/advent_of_code/dec_2024/day_24_input-1.txt")
                  (string/split-lines)))

(def input-2 (->> (slurp "src/advent_of_code/dec_2024/day_24_input-2.txt")
                  (string/split-lines)))

(def test-input-1 ["x00: 1" "x01: 0" "x02: 1" "x03: 1" "x04: 0" "y00: 1" "y01: 1" "y02: 1" "y03: 1" "y04: 1"])
(def test-input-2 ["ntg XOR fgs -> mjb" "y02 OR x01 -> tnw" "kwq OR kpj -> z05" "x00 OR x03 -> fst" "tgd XOR rvg -> z01" "vdt OR tnw -> bfw" "bfw AND frj -> z10" "ffh OR nrd -> bqk" "y00 AND y03 -> djm" "y03 OR y00 -> psh" "bqk OR frj -> z08" "tnw OR fst -> frj" "gnj AND tgd -> z11" "bfw XOR mjb -> z00" "x03 OR x00 -> vdt" "gnj AND wpb -> z02" "x04 AND y00 -> kjc" "djm OR pbm -> qhw" "nrd AND vdt -> hwm" "kjc AND fst -> rvg" "y04 OR y02 -> fgs" "y01 AND x02 -> pbm" "ntg OR kjc -> kwq" "psh XOR fgs -> tgd" "qhw XOR tgd -> z09" "pbm OR djm -> kpj" "x03 XOR y03 -> ffh" "x00 XOR y04 -> ntg" "bfw OR bqk -> z06" "nrd XOR fgs -> wpb" "frj XOR qhw -> z04" "bqk OR frj -> z07" "y03 OR x01 -> nrd" "hwm AND bqk -> z03" "tgd XOR rvg -> z12" "tnw OR pbm -> gnj"])

(defn create-state
  [input-1 input-2]
  (let [state (reduce (fn [state i]
                        (let [[k v] (string/split i #": ")]
                          (assoc state k {:output (edn/read-string v)})))
                      {}
                      input-1)]
    (reduce (fn [state i]
              (let [[b a] (string/split i #" -> ")
                    [i1 gate i2] (string/split b #" ")]
                (assoc state a {:output nil
                                :gate   gate
                                :inputs [i1 i2]})))
            state
            input-2)))

(def test-state (create-state test-input-1 test-input-2))
(def state (create-state input-1 input-2))

(defn get-input-values
  {:test (fn []
           (is= (get-input-values test-state "x00")
                :done)
           (is= (get-input-values test-state "fst")
                [1 1]))}
  [state wire]
  (if (get-in state [wire :output])
    :done
    (->> (get-in state [wire :inputs])
         (reduce (fn [input-values input]
                   (if-let [input-value (get-in state [input :output])]
                     (conj input-values input-value)
                     (reduced :fail)))
                 []))))

(defn apply-gate
  [gate input-values]
  (case gate
    "XOR" (apply bit-xor input-values)
    "OR" (apply max input-values)
    "AND" (apply min input-values)))

(defn pulse
  [state]
  (->> state
       (reduce-kv (fn [state wire v]
                    (let [update-values (get-input-values state wire)]
                      (if (keyword? update-values)
                        state
                        (let [output-value (apply-gate (:gate v) update-values)]
                          (assoc-in state [wire :output] output-value)))))
                  state)))

(defn pulse-until-the-end
  [state]
  (loop [state state]
    (let [new-state (pulse state)]
      (if (= state new-state)
        state
        (recur new-state)))))

(def test-pulsed-state (pulse-until-the-end test-state))
(def pulsed-state (pulse-until-the-end state))

(defn find-decimal
  [state]
  (as-> state $
        (seq $)
        (filter (fn [[k _]] (string/starts-with? k "z")) $)
        (sort-by first $)
        (reverse $)
        (map (fn [[_ v]] (:output v)) $)
        (apply str $)
        (new BigInteger $ 2)))

(comment
  (def test-decimal (find-decimal test-pulsed-state))
  (def decimal (find-decimal pulsed-state))
  )



