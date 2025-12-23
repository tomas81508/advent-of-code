(ns advent-of-code.dec-2025.day-06
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def input (->> (slurp "src/advent_of_code/dec_2025/day_06_input.txt")
                (string/split-lines)))

(def test-input ["123 328  51 64"
                 " 45 64  387 23"
                 "  6 98  215 314"
                 "*   +   *   +"])

(defn calculate-homework
  {:test (fn []
           (is= (calculate-homework test-input)
                4277556))}
  [input]
  (let [operations (->> (re-seq #"[*+]+" (last input))
                        (mapv edn/read-string))
        numbers (->> (drop-last input)
                     (mapv (fn [ns] (mapv edn/read-string (re-seq #"\d+" ns)))))]
    (reduce (fn [sum index]
              (let [operation (get operations index)]
                (+ sum (->> numbers
                            (map (fn [ns] (get ns index)))
                            (apply (eval operation))))))
            0
            (range (count (first numbers))))))

(comment
  (time (calculate-homework input))
  ; "Elapsed time: 5.235167 msecs"
  ; => 6169101504608
  )

; part 2

(defn cephalopod-math
  {:test (fn []
           (is= (cephalopod-math test-input)
                3263827))}
  [input]
  (let [operations (->> (re-seq #"[*+]+" (last input))
                        (mapv edn/read-string))
        digit-strings (drop-last input)
        index-bound (apply max (map count input))
        numbers (-> (reduce (fn [a index]
                              (let [number-as-string (->> (mapv (fn [ds] (get ds index)) digit-strings)
                                                          (apply str)
                                                          (string/trim))]
                                (if (= number-as-string "")
                                  (-> a
                                      (update :current inc)
                                      (update :numbers conj []))
                                  (update-in a [:numbers (:current a)] conj (read-string number-as-string)))))
                            {:current 0
                             :numbers [[]]}
                            (range index-bound))
                    (:numbers))]
    (reduce (fn [sum index]
              (+' sum (apply (eval (get operations index)) (get numbers index))))
            0
            (range (count operations)))))

(comment
  (time (cephalopod-math input))
  ; "Elapsed time: 5.404417 msecs"
  ; => 10442199710797
  )

