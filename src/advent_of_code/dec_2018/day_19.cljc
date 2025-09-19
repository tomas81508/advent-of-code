(ns advent-of-code.dec-2018.day-19
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]
            [clojure.edn :as edn]
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
  [{opcode :opcode args :args} registers]
  (let [operation (get-operation opcode)]
    (operation registers args)))

(defn run-program
  {:test (fn []
           (is= (run-program [{:opcode "seti" :args [5 0 1]}]
                             0
                             [0 0 0 0 0 0])
                {:registers [0 5 0 0 0 0] :status :stopped})
           (is= (run-program [{:opcode "seti" :args [5 0 1]}
                              {:opcode "seti" :args [6 0 2]}
                              {:opcode "addi" :args [0 1 0]}
                              {:opcode "addr" :args [1 2 3]}
                              {:opcode "setr" :args [1 0 0]}
                              {:opcode "seti" :args [8 0 4]}
                              {:opcode "seti" :args [9 0 5]}]
                             0
                             [0 0 0 0 0 0])
                {:registers [6 5 6 0 0 9] :status :stopped}))}
  [program ip-bound registers]
  (loop [i 0
         ip (get registers ip-bound)
         registers registers]
    (let [instruction (get program ip)]
      (cond (not instruction)
            {:registers registers :status :stopped}

            (> i 50000000)
            {:status :forced-stopped}

            :else
            (let [registers (->> (assoc registers ip-bound ip)
                                 (run-instruction instruction))]
              (recur (inc i)
                     (inc (get registers ip-bound))
                     registers))))))

(defn read-program
  [src]
  (let [lines (->> src
                   (string/split-lines))
        instructions (->> lines
                          (drop 1)
                          (map (fn [line]
                                 {:opcode (subs line 0 4)
                                  :args   (as-> (subs line 5) $
                                                (clojure.string/split $ #" ")
                                                (map edn/read-string $))}))
                          (into []))
        ip-bound (as-> lines $
                       (take 1 $)
                       (first $)
                       (subs $ 4)
                       (edn/read-string $))]
    {:ip-bound ip-bound :instructions instructions}))

(defn solve-puzzle-a []
  (time (let [{ip-bound :ip-bound instructions :instructions}
              (read-program "src/advent_of_code/dec_2018/day_19.txt")]
          (run-program instructions ip-bound [0 0 0 0 0 0]))))

; "Elapsed time: 7460.002189 msecs"
; => {:registers [978 978 977 1 978 256], :status :stopped}

; Not a real solution
(comment
  (let [huge-number 10551377]
    (->> (for [n (range 1 (inc huge-number))
               :when (zero? (mod huge-number n))]
           n)
         (apply +))))