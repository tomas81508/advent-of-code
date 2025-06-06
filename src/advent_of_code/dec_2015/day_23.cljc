(ns advent-of-code.dec-2015.day-23
  (:require [advent-of-code.test :refer [is=]]
            [clojure.edn :as edn]))

(def input (->> (slurp "src/advent_of_code/dec_2015/day_23_input.txt")
                (clojure.string/split-lines)))

(defmulti run-instruction
          (fn [instruction _]
            (subs instruction 0 3)))

(defmethod run-instruction
  "hlf"
  [instruction variables]
  (let [variable (subs instruction 4 5)]
    {:variables (update variables variable / 2)}))

(defmethod run-instruction
  "tpl"
  [instruction variables]
  (let [variable (subs instruction 4 5)]
    {:variables (update variables variable * 3)}))

(defmethod run-instruction
  "inc"
  [instruction variables]
  (let [variable (subs instruction 4 5)]
    {:variables (update variables variable inc)}))

(defmethod run-instruction
  "jmp"
  [instruction variables]
  (let [offset (edn/read-string (subs instruction 4))]
    {:index-jump offset
     :variables variables}))

(defmethod run-instruction
  "jie"
  [instruction variables]
  (let [variable (subs instruction 4 5)
        offset (edn/read-string (subs instruction 7))]
    {:index-jump (when (even? (get variables variable)) offset)
     :variables variables}))

(defmethod run-instruction
  "jio"
  [instruction variables]
  (let [variable (subs instruction 4 5)
        offset (edn/read-string (subs instruction 7))]
    {:index-jump (when (= 1 (get variables variable)) offset)
     :variables variables}))

(defn run-instructions
  {:test (fn []
           (is= (run-instructions ["inc a" "jio a, +2" "tpl a" "inc a"] {"a" 0 "b" 0})
                {"a" 2
                 "b" 0}))}
  [instructions variables]
  (loop [index 0
         variables variables]
    (let [instruction (get instructions index :exit)]
      (if (= instruction :exit)
        variables
        (let [{index-jump :index-jump variables :variables} (run-instruction instruction variables)]
          (recur (if index-jump (+ index index-jump) (inc index))
                 variables))))))

(comment
  (time (run-instructions input {"a" 0 "b" 0}))
  (time (run-instructions input {"a" 1 "b" 0}))
  )