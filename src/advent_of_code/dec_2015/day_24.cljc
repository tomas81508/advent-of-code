(ns advent-of-code.dec-2015.day-24
  (:require [advent-of-code.test :refer [is=]]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.edn :as edn]))

(def input (->> (slurp "src/advent_of_code/dec_2015/day_24_input.txt")
                (clojure.string/split-lines)
                (map edn/read-string)))

(def test-input [1 2 3 4 5 7 8 9 10 11])

(defn find-groups
  {:test (fn []
           (is= (find-groups test-input 20)
                #{#{11 9} #{11 8 1} #{11 7 2} #{11 5 4} #{11 5 3 1} #{11 4 3 2}
                  #{10 9 1} #{10 8 2} #{10 7 3} #{10 7 2 1} #{10 5 4 1} #{10 5 3 2} #{10 4 3 2 1}
                  #{9 8 3} #{9 8 2 1} #{9 7 4} #{9 7 3 1} #{9 5 4 2} #{9 5 3 2 1}
                  #{8 7 5} #{8 7 4 1} #{8 7 3 2} #{8 5 4 3} #{8 5 4 2 1}
                  #{7 1 4 3 5}})
           (is= (find-groups [1 2 3 4 5 7 8 10] 20)
                #{#{10 8 2} #{10 7 3} #{10 7 2 1} #{10 5 4 1} #{10 5 3 2} #{10 4 3 2 1}
                  #{8 7 5} #{8 7 4 1} #{8 7 3 2} #{8 5 4 3} #{8 5 4 2 1}
                  #{7 1 4 3 5}}))}
  [weights sum]
  (let [weights (reverse weights)]
    (loop [result #{}
           current [[0 #{} weights]]]
      (println "count" (count current))
      (if (empty? current)
        result
        (let [{result  :result
               current :current} (->> current
                                      (map (fn [[part-result terms weights]]
                                             (->> weights
                                                  (drop-while (fn [x] (> (+ x part-result) sum)))
                                                  (map (fn [x] [(+ part-result x)
                                                                (conj terms x)
                                                                (->> weights
                                                                     (drop-while (fn [n] (>= n x))))])))))
                                      (reduce (fn [a v] (apply conj a v)) [])
                                      (reduce (fn [a [part-result terms weights]]
                                                (cond (= part-result sum)
                                                      (update a :result conj terms)

                                                      (empty? weights)
                                                      a

                                                      :else
                                                      (update a :current conj [part-result terms weights])))
                                              {:result  result
                                               :current []}))
              ]
          (recur result current))))))

(comment
  (def groups-3 (time (find-groups input (/ (apply + input) 3))))
  ; "Elapsed time: 18531.4325 msecs"
  ; => #'advent-of-code.dec-2015.day-24/groups-3
  (def groups-4 (time (find-groups input (/ (apply + input) 4))))
  ; "Elapsed time: 2496.480208 msecs"
  ; => #'advent-of-code.dec-2015.day-24/groups-4
  )

(defn find-groups-of
  {:test (fn []
           (is= (find-groups-of test-input 3)
                #{#{#{3 2 5 10} #{7 4 9} #{1 11 8}}
                  #{#{4 3 2 11} #{1 9 10} #{7 5 8}}
                  #{#{7 1 2 10} #{11 9} #{4 3 5 8}}
                  #{#{7 3 2 8} #{1 9 10} #{4 11 5}}
                  #{#{4 3 5 8} #{1 9 10} #{7 2 11}}
                  #{#{3 2 5 10} #{11 9} #{7 1 4 8}}
                  #{#{11 9} #{1 4 3 2 10} #{7 5 8}}
                  #{#{2 10 8} #{7 1 3 9} #{4 11 5}}
                  #{#{7 3 10} #{1 11 8} #{4 2 9 5}}
                  #{#{1 4 2 5 8} #{7 3 10} #{11 9}}
                  #{#{1 3 11 5} #{7 4 9} #{2 10 8}}
                  #{#{7 1 2 10} #{3 9 8} #{4 11 5}}
                  #{#{7 3 2 8} #{11 9} #{1 4 5 10}}
                  #{#{1 4 5 10} #{3 9 8} #{7 2 11}}
                  #{#{7 1 4 3 5} #{11 9} #{2 10 8}}
                  #{#{7 3 10} #{4 11 5} #{1 2 9 8}}})
           )}
  ([weights number-of-groups]
   (find-groups-of weights
                   number-of-groups
                   (find-groups weights (/ (apply + weights) number-of-groups))))
  ([_ number-of-groups groups]
   (->> (combinatorics/combinations groups number-of-groups)
        (filter (fn [gs] (= (reduce + (->> gs (map count)))
                            (count (apply clojure.set/union gs)))))
        (map (fn [gs] (into #{} gs)))
        (into #{}))))

(defn find-quantum-entanglement
  {:test (fn []
           (is= (find-quantum-entanglement test-input 3)
                99))}
  ([weights number-of-groups]
   (find-quantum-entanglement weights
                              number-of-groups
                              (find-groups weights (/ (apply + weights) number-of-groups))))
  ([weights _ groups]
   (let [smallest-groups (->> groups
                              (reduce (fn [a g]
                                        (cond (< (count g) (:size a))
                                              {:groups [g] :size (count g)}
                                              (= (count g) (:size a))
                                              (update a :groups conj g)
                                              :else a))
                                      {:groups []
                                       :size   (count weights)})
                              (:groups)
                              (map (fn [g] {:group g :quantum-entanglement (apply * g)}))
                              (sort-by :quantum-entanglement))]
     (-> smallest-groups
         (first)
         (:quantum-entanglement)))))

  (comment

    (time (find-quantum-entanglement input 3 groups-3))
    ; "Elapsed time: 66.966792 msecs"
    ; => 11846773891
    (time (find-quantum-entanglement input 4 groups-4))
    ; "Elapsed time: 13.544542 msecs"
    ; => 80393059
    )
