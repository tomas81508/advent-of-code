(ns advent-of-code.dec-2018.day-08
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2018/day_08.txt") $
        (string/replace $ #"\n" "")
        (string/split $ #" ")
        (map read-string $)))


(declare get-node)


(defn get-children [number-of-children numbers]
  (reduce (fn [[children numbers] _]
            (let [[node numbers] (get-node numbers)]
              [(conj children node) numbers]))
          [[] numbers]
          (range number-of-children)))


(defn get-metadata [metadata numbers]
  [(take metadata numbers) (drop metadata numbers)])


(defn get-node
  {:test (fn []
           (is= (get-node [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
                [{:children [{:metadata [10 11 12]}
                             {:children [{:metadata [99]}]
                              :metadata [2]}]
                  :metadata [1 1 2]}
                 []]))}
  [numbers]
  (let [[number-of-children metadata & numbers] numbers
        [children numbers] (get-children number-of-children numbers)
        [metadata numbers] (get-metadata metadata numbers)]
    [(merge {:metadata metadata}
            (when-not (empty? children)
              {:children children}))
     numbers]))


(defn sum-metadata-part-1
  {:test (fn []
           (is= (-> (get-node [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
                    (first)
                    (sum-metadata-part-1))
                138))}
  [node]
  (apply + (concat (:metadata node)
                   (map sum-metadata-part-1 (:children node)))))


(deftest puzzle-part-1
  (is= (->> (get-puzzle-input)
            (get-node)
            (first)
            (sum-metadata-part-1))
       35852))

(def s (atom []))

(defn sum-metadata-part-2
  {:test (fn []
           (is= (-> (get-node [1 1 0 1 99 2])
                    (first)
                    (sum-metadata-part-2))
                0)
           (is= (-> (get-node [0 3 10 11 12])
                    (first)
                    (sum-metadata-part-2))
                33)
           (is= (-> (get-node [0 1 99])
                    (first)
                    (sum-metadata-part-2))
                99)
           (is= (-> (get-node [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])
                    (first)
                    (sum-metadata-part-2))
                66)
           (is= (-> (get-node [1 2 2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2 1 1])
                    (first)
                    (sum-metadata-part-2))
                132))}
  [node]
  (if-not (contains? node :children)
    (apply + (:metadata node))
    (let [children-sums (map sum-metadata-part-2 (:children node))]
      (swap! s conj {:c children-sums :m (:metadata node)})
      (apply + (->> (:metadata node)
                    (map (fn [index]
                           (if (and (pos? index) (<= index (count children-sums)))
                             (nth children-sums (dec index))
                             0))))))))


(deftest puzzle-part-2
  (is= (->> (get-puzzle-input)
            (get-node)
            (first)
            (sum-metadata-part-2))
       33422))

