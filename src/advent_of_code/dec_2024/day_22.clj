(ns advent-of-code.dec-2024.day-22
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

(def input (->> (slurp "src/advent_of_code/dec_2024/day_22_input.txt")
                (string/split-lines)
                (map clojure.edn/read-string)))

(def test-input (->> ["1" "10" "100" "2024"]
                     (map clojure.edn/read-string)))

(defn mix
  {:test (fn []
           (is= (mix 42 15) 37))}
  [secret v]
  (bit-xor secret v))

(defn prune
  {:test (fn []
           (is= (prune 100000000) 16113920))}
  [secret]
  (mod secret 16777216))

(defn evolve
  {:test (fn []
           (is= (evolve 123) 15887950)
           (is= ((apply comp (repeat 2 evolve)) 123) 16495136)
           (is= (as-> (range 1 11) $
                      (map (fn [n] (apply comp (repeat n evolve))) $)
                      ((apply juxt $) 123))
                [15887950 16495136 527345 704524 1553684 12683156 11100544 12249484 7753432 5908254]))
   }
  [secret]
  (let [secret (-> secret
                   (* 64)
                   (mix secret)
                   (prune))
        secret (-> secret
                   (/ 32)
                   (int)
                   (mix secret)
                   (prune))]
    (-> secret
        (* 2048)
        (mix secret)
        (prune))))

(defn solve-1a
  {:test (fn []
           (is= (solve-1a test-input)
                37327623))}
  [secret-numbers]
  (->> (reduce (fn [secret-numbers _]
                 (->> secret-numbers
                      (map evolve)))
               secret-numbers
               (range 2000))
       (reduce +)))

(defn evolve-n
  [n]
  (apply comp (repeat n evolve)))

(defn solve-1b
  {:test (fn []
           (is= (solve-1b test-input)
                37327623))}
  [secret-numbers]
  (let [evolve-2000 (evolve-n 2000)]
    (->> secret-numbers
         (map evolve-2000)
         (reduce +))))


(comment
  (time (solve-1a input))
  (time (solve-1b input))
  )

(defn secret-numbers->changes
  {:test (fn []
           (is= (secret-numbers->changes [123 15887950 16495136 527345 704524])
                [-3 6 -1 -1]))}
  [secret-numbers]
  (->> secret-numbers
       (map (fn [n] (mod n 10)))
       (partition 2 1)
       (map (fn [[n1 n2]] (- n2 n1)))))

(defn get-price
  {:test (fn []
           (is= (get-price 123 [-1 -1 0 2]) 6)
           (is= (get-price 1 [-2 1 -1 3]) 7)
           (is= (get-price 2 [-2 1 -1 3]) 7)
           (is= (get-price 3 [-2 1 -1 3]) nil)
           (is= (get-price 2024 [-2 1 -1 3]) 9))}
  [secret-number sequence]
  (loop [secret-numbers [secret-number
                         (evolve secret-number)
                         ((evolve-n 2) secret-number)
                         ((evolve-n 3) secret-number)
                         ((evolve-n 4) secret-number)]
         n 4]
    (let [current-secret-numbers (take-last 5 secret-numbers)
          current-changes (secret-numbers->changes current-secret-numbers)]
      (cond (> n 2000)
            nil

            (= current-changes sequence)
            (-> (last current-secret-numbers)
                (mod 10))

            :else
            (recur (conj secret-numbers (evolve (last secret-numbers)))
                   (inc n))))))




















