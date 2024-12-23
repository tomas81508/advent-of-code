(ns advent-of-code.dec-2024.day-23
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_23_input.txt")
                (string/split-lines)
                (map (fn [x] (string/split x #"-")))))

(def test-input (->> ["kh-tc" "qp-kh" "de-cg" "ka-co" "yn-aq" "qp-ub" "cg-tb" "vc-aq" "tb-ka" "wh-tc" "yn-cg" "kh-ub" "ta-co" "de-co" "tc-td" "tb-wq" "wh-td" "ta-ka" "td-qp" "aq-cg" "wq-ub" "ub-vc" "de-ta" "wq-aq" "wq-vc" "wh-yn" "ka-de" "kh-ta" "co-tc" "wh-qp" "tb-vc" "td-yn"]
                     (map (fn [x] (string/split x #"-")))))

(defn create-state
  [input]
  (as-> input $
        (reduce (fn [a [c1 c2]]
                  (-> a
                      (update c1 conj c2)
                      (update c2 conj c1)))
                {}
                $)
        (update-vals $ (fn [v] (into #{} v)))))

(def test-state (create-state test-input))
(def state (create-state input))

(defn cluster-of-three
  {:test (fn []
           (is= (count (cluster-of-three test-state "ta")) 3))}
  [state c]
  (->> (for [c1 (get state c)
             c2 (get state c1)
             :when (contains? (get state c2) c)]
         #{c c1 c2})
       (into #{})))

(defn solve-one
  {:test (fn []
           (is= (solve-one test-state) 7))}
  [state]
  (->> state
       (keys)
       (filter (fn [c] (string/starts-with? c "t")))
       (reduce (fn [a c]
                 (println c)
                 (let [clusters (cluster-of-three (:state a) c)]
                   (-> a
                       (update :state dissoc c)
                       (update :result (fn [result] (apply conj result clusters))))))
               {:state  state
                :result #{}})
       (:result)
       (count)))

(comment
  (time (solve-one state))
  )

(defn get-cluster
  [state c]
  (loop [result-cluster #{c}
         boundary (get state c)]
    ))









