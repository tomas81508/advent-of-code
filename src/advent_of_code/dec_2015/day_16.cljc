(ns advent-of-code.dec-2015.day-16
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.edn :as edn]))

(def input (-> (slurp "src/advent_of_code/dec_2015/day_16_input.txt")
               (clojure.string/split-lines)))

(def test-input [])

(defn parse-line
  {:test (fn []
           (is= (parse-line "Sue 1: children: 1, cars: 8, vizslas: 7")
                {:name     "Sue 1"
                 :children 1
                 :cars     8
                 :vizslas  7}))}
  [text]
  (let [[_ n r] (re-find #"(Sue \d+): (.+)" text)]
    (->> (clojure.string/split r #", ")
         (map (fn [x] (clojure.string/split x #": ")))
         (reduce (fn [a [k v]]
                   (println a k v)
                   (assoc a (keyword k) (edn/read-string v)))
                 {:name n}))))

(def input-state (->> input (map parse-line)))

(def description {:children    3
                  :cats        7
                  :samoyeds    2
                  :pomeranians 3
                  :akitas      0
                  :vizslas     0
                  :goldfish    5
                  :trees       3
                  :cars        2
                  :perfumes    1})

(defn match?
  {:test (fn []
           (is-not (match? {:name "Sue 1" :children 1 :cars 8 :vizslas 7}
                           description))
           (is (match? {:name "Sue 1000" :children 3 :cars 2 :vizslas 0}
                       description)))}
  [aunt-state description]
  (->> (seq description)
       (every? (fn [[k v]]
                 (let [aunt-value (get aunt-state k)]
                   (or (nil? aunt-value)
                       (= aunt-value v)))))))

(defn find-aunt
  [input-state description]
  (->> input-state
       (filter (fn [aunt-state] (match? aunt-state description)))))

(comment
  (find-aunt input-state description)
  )

; part 2

(defn match-2?
  [aunt-state description]
  (->> (seq description)
       (every? (fn [[k v]]
                 (let [aunt-value (get aunt-state k)]
                   (or (nil? aunt-value)
                       (cond (contains? #{:cats :trees} k)
                             (> aunt-value v)

                             (contains? #{:pomeranians :goldfish} k)
                             (< aunt-value v)

                             :else
                             (= aunt-value v))))))))

(defn find-aunt-2
  [input-state description]
  (->> input-state
       (filter (fn [aunt-state] (match-2? aunt-state description)))))

(comment
  (find-aunt-2 input-state description)
  )

