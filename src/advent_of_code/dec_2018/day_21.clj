(ns advent-of-code.dec-2018.day-21
  (:require [advent-of-code.test :refer [is=]]
            [clojure.core]
            [clojure.edn :as edn]
            [advent-of-code.dec-2018.day-19 :as day-19]
            [clojure.set :refer [union]]
            [clojure.string :as string]))

(def input-program (->> (slurp "src/advent_of_code/dec_2018/day_21.txt")
                        (string/split-lines)
                        (drop 1)
                        (mapv (fn [l]
                                (let [[op & args] (string/split l #" ")]
                                  {:opcode op
                                   :args   (mapv edn/read-string args)})))))

(defn run-program
  [program ip-bound registers]
  (loop [ip (get registers ip-bound)
         registers registers
         values #{}
         last-value nil]
    (let [instruction (get program ip)]
      (cond (not instruction)
            {:registers registers :status :stopped}

            :else
            (let [registers (->> (assoc registers ip-bound ip)
                                 (day-19/run-instruction instruction))]
              (if (= (:opcode instruction) "eqrr")
                (do (println (get registers 4) (count values))
                  (if (contains? values (get registers 4))
                    last-value
                    (recur (inc (get registers ip-bound))
                           registers
                           (conj values (get registers 4))
                           (get registers 4))))
                (recur (inc (get registers ip-bound))
                       registers
                       values
                       last-value)))))))

(comment
  ; debugging to see when eqrr 4 0 3 is called. The registers look like this
  ; [1 1 28 1 10720163 1]
  (run-program input-program 2 [10720163 0 0 0 0 0])

  ; I need to detect a loop
  (run-program input-program 2 [0 0 0 0 0 0])
  )
