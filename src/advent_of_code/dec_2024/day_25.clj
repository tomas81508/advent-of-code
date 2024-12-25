(ns advent-of-code.dec-2024.day-25
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

(defn map-with-character
  [thing character]
  (->> thing
       (drop 1)
       (into [])
       (reduce-kv (fn [a level row]
                    (->> (into [] row)
                         (reduce-kv (fn [a index c]
                                      (if (= c character) (update a index (fn [v] (or v level))) a))
                                    a)))
                  [nil nil nil nil nil])))

(defn map-key
  {:test (fn []
           (is= (map-key ["....." "#...." "#...." "##..#" "##.##" "#####" "#####"])
                [5 3 1 2 3]))}
  [key]
  (->> (map-with-character key \#)
       (map (fn [x] (- 5 x)))))

(defn map-lock
  {:test (fn []
           (is= (map-lock ["#####" ".####" ".####" ".####" ".#.#." ".#..." "....."])
                [0 5 3 4 3]))}
  [lock]
  (map-with-character lock \.))


(def state (as-> (slurp "src/advent_of_code/dec_2024/day_25_input.txt") $
                 (string/split $ #"\n\n")
                 (map string/split-lines $)
                 (reduce (fn [state thing]
                           (if (= (ffirst thing) \#)
                             (update state :locks conj (map-lock thing))
                             (update state :keys conj (map-key thing))))
                         {:locks #{}
                          :keys  #{}}
                         $)))

(defn test-key-with-lock
  {:test (fn []
           (is-not (test-key-with-lock [5 0 2 1 3] [0 5 3 4 3]))
           (is-not (test-key-with-lock [4 3 4 0 2] [0 5 3 4 3]))
           (is (test-key-with-lock [3 0 2 0 1] [0 5 3 4 3]))
           (is-not (test-key-with-lock [5 0 2 1 3] [1 2 0 5 3]))
           (is (test-key-with-lock [4 3 4 0 2] [1 2 0 5 3]))
           (is (test-key-with-lock [3 0 2 0 1] [1 2 0 5 3])))}
  [key lock]
  (->> (map + key lock)
       (some (fn [x] (> x 5)))
       (not)))

(comment
  (loop [keys (:keys state)
         pairs 0]
    (if (empty? keys)
      pairs
      (let [key (first keys)
            matching-locks (->> (:locks state)
                                (filter (fn [l] (test-key-with-lock key l)))
                                (count))]
        (recur (disj keys key)
               (+ pairs matching-locks)))))

  ; too low 238

  )



