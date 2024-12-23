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

(defn get-price-for-secret
  {:test (fn []
           (is= (get-price-for-secret 16495136) 6))}
  [secret]
  (mod secret 10))

(defn get-changes->price-for-secret
  {:test (fn []
           (is= (-> (get-changes->price-for-secret 1)
                    (get [-2 1 -1 3]))
                7)
           (is= (-> (get-changes->price-for-secret 2)
                    (get [-2 1 -1 3]))
                7)
           (is= (-> (get-changes->price-for-secret 3)
                    (get [-2 1 -1 3]))
                nil)
           (is= (-> (get-changes->price-for-secret 2024)
                    (get [-2 1 -1 3]))
                9))}
  [secret]
  (loop [n 0
         secret secret
         prices [(get-price-for-secret secret)]
         change-sequence []
         result {}]
    (if (= n 2000)
      result
      (let [new-secret (evolve secret)
            new-secret-price (get-price-for-secret new-secret)
            change (- new-secret-price (last prices))
            change-sequence (->> (conj change-sequence change)
                                 (take-last 4)
                                 (into []))]
        (recur (inc n)
               new-secret
               (conj prices new-secret-price)
               change-sequence
               (if (or (contains? result change-sequence)
                       (< (count change-sequence) 4))
                 result
                 (assoc result change-sequence new-secret-price)))))))

(def test-input-2 [1
                   2
                   3
                   2024])

(def all-changes->price (map get-changes->price-for-secret input))
(def test-all-changes->price (map get-changes->price-for-secret test-input-2))

(comment
  (time (->> (for [a (range -9 10)
                   b (range -9 10)
                   c (range -9 10)
                   d (range -9 10)]
               [a b c d])
             (map (fn [change-sequence]
                    (when (= (rest change-sequence) [-9 -9 -9])
                      (println change-sequence))
                    (reduce (fn [a change->price]
                              ;(when (not (zero? (get change->price change-sequence 0)))
                              ;  (println (get change->price change-sequence 0)))
                              (+ a (get change->price change-sequence 0)))
                            0
                            all-changes->price)))
             (apply max)))
  ; "Elapsed time: 249918.532044 msecs"
  ; => 1582

  )
















