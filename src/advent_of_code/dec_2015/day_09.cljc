(ns advent-of-code.dec-2015.day-09
  (:require [ysera.test :refer [deftest is=]]
            [clojure.string :refer [split-lines]]
            [clojure.set :refer [union]]
            [clojure.math.combinatorics :refer [permutations]]))


(defn get-puzzle-input
  []
  (->> (slurp "src/advent_of_code/dec_2015/day_09_input.txt")
       (split-lines)))

(def test-input ["London to Dublin = 464"
                 "London to Belfast = 518"
                 "Dublin to Belfast = 141"])


(defn parse-line
  {:test (fn []
           (is= (parse-line "Tristram to AlphaCentauri = 34")
                {:cities   #{"AlphaCentauri" "Tristram"}
                 :distance 34}))}
  [s]
  (let [pattern (re-pattern "([^ ]+) to ([^ ]+) = (\\d+)")
        [_ city1 city2 distance] (re-matches pattern s)]
    {:cities   #{city1 city2}
     :distance (read-string distance)}))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:cities    #{"Belfast" "Dublin" "London"}
                 :distances {#{"Belfast" "Dublin"} 141
                             #{"Belfast" "London"} 518
                             #{"Dublin" "London"}  464}}))}
  [lines]
  (->> lines
       (map parse-line)
       (reduce (fn [state {cities :cities distance :distance}]
                 (-> state
                     (update :cities union cities)
                     (update :distances assoc cities distance)))
               {})))


(defn distance
  {:test (fn []
           (is= (-> (create-state test-input)
                    (distance "Dublin" "London"))
                464))}
  [state city1 city2]
  (get-in state [:distances #{city1 city2}]))


(defn calculate-distance
  {:test (fn []
           (is= (-> (create-state test-input)
                    (calculate-distance ["Belfast" "Dublin" "London"]))
                605))}
  [state cities]
  (loop [[c1 c2 :as cities] cities
         total-distance 0]
    (if-not c2
      total-distance
      (recur (drop 1 cities)
             (+ total-distance (distance state c1 c2))))))


(defn calculate-min-distance
  {:test (fn []
           (is= (-> (create-state test-input)
                    (calculate-min-distance))
                605))}
  [state]
  (->> (permutations (:cities state))
       (map (partial calculate-distance state))
       (reduce min)))

(deftest puzzle-a
         (is= (time (-> (get-puzzle-input)
                        (create-state)
                        (calculate-min-distance)))
              ; "Elapsed time: 335.934565 msecs"
              251))

(defn calculate-max-distance
  {:test (fn []
           (is= (-> (create-state test-input)
                    (calculate-max-distance))
                982))}
  [state]
  (->> (permutations (:cities state))
       (map (partial calculate-distance state))
       (reduce max)))

(deftest puzzle-a
         (is= (time (-> (get-puzzle-input)
                        (create-state)
                        (calculate-max-distance)))
              ; "Elapsed time: 294.473778 msecs"
              898))







