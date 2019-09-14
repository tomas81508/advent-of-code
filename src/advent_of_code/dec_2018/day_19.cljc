(ns advent-of-code.dec-2018.day-19
  (:require [ysera.test :refer [is=]]
            [advent-of-code.dec-2018.day-16 :as day-16]))

(def get-operation {"addr" day-16/addr
                    "addi" day-16/addi
                    "mulr" day-16/mulr
                    "muli" day-16/muli
                    "banr" day-16/banr
                    "bani" day-16/bani
                    "borr" day-16/borr
                    "bori" day-16/bori
                    "setr" day-16/setr
                    "seti" day-16/seti
                    "gtir" day-16/gtir
                    "gtri" day-16/gtri
                    "gtrr" day-16/gtrr
                    "eqir" day-16/eqir
                    "eqri" day-16/eqri
                    "eqrr" day-16/eqrr})

(defn run-instruction
  {:test (fn []
           (is= (run-instruction {:opcode "seti" :args [5 0 1]}
                                 [0 0 0 0 0 0])
                [0 5 0 0 0 0])
           (is= (run-instruction {:opcode "seti" :args [6 0 2]}
                                 [1 5 0 0 0 0])
                [1 5 6 0 0 0])
           (is= (run-instruction {:opcode "addi" :args [0 1 0]}
                                 [2 5 6 0 0 0])
                [3 5 6 0 0 0]))}
  [{opcode :opcode args :args} register]
  (let [operation (get-operation opcode)]
    (operation register args)))

(defn run-program
  {:test (fn []
           (is= (run-program [{:opcode "seti" :args [5 0 1]}]
                             [0 0 0 0 0 0])
                [1 5 0 0 0 0])
           (is= (run-program [{:opcode "seti" :args [5 0 1]}
                              {:opcode "seti" :args [6 0 2]}
                              {:opcode "addi" :args [0 1 0]}
                              {:opcode "addr" :args [1 2 3]}
                              {:opcode "setr" :args [1 0 0]}
                              {:opcode "seti" :args [8 0 4]}
                              {:opcode "seti" :args [9 0 5]}]
                             [0 0 0 0 0 0])
                [7 5 6 0 0 9]))}
  [program [ip & _ :as register]]
  (let [instruction (get program ip)]
    (println instruction register)
    (if-not instruction
      register
      (recur program
             (-> (run-instruction instruction register)
                 (update 0 inc))))))

(defn solve-puzzle-a []
  (let [program (->> (slurp "src/advent_of_code/dec_2018/day_19.txt")
                     (clojure.string/split-lines)
                     (drop 1)
                     (map (fn [line]
                            {:opcode (subs line 0 4)
                             :args   (as-> (subs line 5) $
                                           (clojure.string/split $ #" ")
                                           (map read-string $))}))
                     (into []))
        ip (as-> (slurp "src/advent_of_code/dec_2018/day_19.txt") $
                 (clojure.string/split-lines $)
                 (take 1 $)
                 (first $)
                 (subs $ 4)
                 (read-string $))]
    (run-program program [ip 0 0 0 0 0])))
