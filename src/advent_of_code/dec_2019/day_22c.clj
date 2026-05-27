(ns advent-of-code.dec-2019.day-22c
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def puzzle-input (->> (slurp "src/advent_of_code/dec_2019/day_22.txt")
                       (string/split-lines)))


(def deck-size-a 10007)

;; Part 1
;; Here we track the card at position p and how it moves for each instruction

(defn deal-into-new-stack
  {:test (fn []
           (is= (deal-into-new-stack 2 10) 7)
           (is= (deal-into-new-stack 0 10) 9)
           (is= (deal-into-new-stack 9 10) 0))}
  [p deck-size]
  (- deck-size p 1))

(defn cut-N-cards
  {:test (fn []
           (is= (cut-N-cards 5 10 3) 2)
           (is= (cut-N-cards 1 10 3) 8)
           (is= (cut-N-cards 5 10 -4) 9))}
  [p deck-size N]
  (mod (- p N) deck-size))

(defn deal-with-increment-N
  {:test (fn []
           (is= (deal-with-increment-N 2 7 3) 6)
           (is= (deal-with-increment-N 4 7 3) 5))}
  [p deck-size N]
  (mod (* p N) deck-size))

(defn perform-instruction
  [p deck-size instruction]
  (cond (string/starts-with? instruction "deal into new stack")
        (deal-into-new-stack p deck-size)

        (string/starts-with? instruction "deal with")
        (let [N (edn/read-string (re-find #"\d+" instruction))]
          (deal-with-increment-N p deck-size N))

        :else
        (let [N (edn/read-string (re-find #"-?\d+" instruction))]
          (cut-N-cards p deck-size N))))

(deftest puzzle-a
  (is= (let [position 2019]
         (->> puzzle-input
              (reduce (fn [p instruction] (perform-instruction p deck-size-a instruction))
                      position)))
       6417))

;; Part 1 - inverse 6417 -> 2019
;; The same, but for the inverse of the above

(defn cut-N-cards-inverse
  {:test (fn []
           (is= (cut-N-cards-inverse 2 10 3) 5)
           (is= (cut-N-cards-inverse 8 10 3) 1)
           (is= (cut-N-cards-inverse 9 10 -4) 5))}
  [p deck-size N]
  (mod (+ p N) deck-size))

(defn mod-inverse
  "Returns a^-1 mod p, or throws if inverse doesn't exist."
  [a p]
  (let [A (bigint a)
        P (bigint p)]
    (.modInverse (biginteger A) (biginteger P))))

(defn deal-with-increment-N-inverse
  {:test (fn []
           (is= (deal-with-increment-N-inverse 6 7 3) 2)
           (is= (deal-with-increment-N-inverse 5 7 3) 4))}
  [p deck-size N]
  (mod (* p (mod-inverse N deck-size)) deck-size))

(defn perform-inverse-instruction
  [p deck-size instruction]
  (cond (string/starts-with? instruction "deal into new stack")
        (deal-into-new-stack p deck-size)

        (string/starts-with? instruction "deal with")
        (let [N (edn/read-string (re-find #"\d+" instruction))]
          (deal-with-increment-N-inverse p deck-size N))

        :else
        (let [N (edn/read-string (re-find #"-?\d+" instruction))]
          (cut-N-cards-inverse p deck-size N))))

(deftest puzzle-a-inverse
  (is= (let [position 6417]
         (->> puzzle-input
              (reverse)
              (reduce (fn [p instruction] (perform-inverse-instruction p deck-size-a instruction))
                      position)))
       2019))

;; Part 1 - with linear function composition
;; Since compositions of linear functions are a linear function we track down the linear function that shuffles all instructions

(defn deal-into-new-stack-coefficients
  {:test (fn []
           (is= (deal-into-new-stack-coefficients 10 [1 0]) [-1 9]))}
  [deck-size [a b]]
  [(- a) (- deck-size b 1)])

(defn cut-N-cards-coefficients
  {:test (fn []
           (is= (cut-N-cards-coefficients 3 [1 0]) [1 -3]))}
  [N [a b]]
  [a (- b N)])

(defn deal-with-increment-N-coefficients
  {:test (fn []
           (is= (deal-with-increment-N-coefficients 7 3 [1 0]) [3 0]))}
  [deck-size N [a b]]
  [(mod (*' N a) deck-size) (mod (*' N b) deck-size)])

(defn perform-instruction-to-function-coefficients
  [deck-size coefficients instruction]
  (cond (string/starts-with? instruction "deal into new stack")
        (deal-into-new-stack-coefficients deck-size coefficients)

        (string/starts-with? instruction "deal with")
        (let [N (edn/read-string (re-find #"\d+" instruction))]
          (deal-with-increment-N-coefficients deck-size N coefficients))

        :else
        (let [N (edn/read-string (re-find #"-?\d+" instruction))]
          (cut-N-cards-coefficients N coefficients))))

(defn get-shuffle-transformation
  [deck-size instructions]
  (->> instructions
       (reduce (fn [coefficients instruction]
                 (perform-instruction-to-function-coefficients deck-size coefficients instruction))
               [1 0])
       (map (fn [c] (mod c deck-size)))))

(deftest puzzle-a-with-linear-functions
  (is= (let [position 2019
             [a b] (get-shuffle-transformation deck-size-a puzzle-input)]
         (mod (+ (* a position) b) deck-size-a))
       6417))

;; Part 1 - inverse with linear function composition

(defn cut-N-cards-inverse-coefficients
  {:test (fn []
           (is= (cut-N-cards-inverse-coefficients 3 [1 -3]) [1 0]))}
  [N [a b]]
  [a (+ b N)])

(defn deal-with-increment-N-inverse-coefficients
  {:test (fn []
           (is= (deal-with-increment-N-inverse-coefficients 7 3 [3 0]) [1 0]))}
  [deck-size N [a b]]
  (let [N-inverse (mod-inverse N deck-size)]
    [(mod (*' N-inverse a) deck-size) (mod (*' N-inverse b) deck-size)]))

(defn perform-inverse-instruction-to-function-coefficients
  [deck-size coefficients instruction]
  (cond (string/starts-with? instruction "deal into new stack")
        (deal-into-new-stack-coefficients deck-size coefficients)

        (string/starts-with? instruction "deal with")
        (let [N (edn/read-string (re-find #"\d+" instruction))]
          (deal-with-increment-N-inverse-coefficients deck-size N coefficients))

        :else
        (let [N (edn/read-string (re-find #"-?\d+" instruction))]
          (cut-N-cards-inverse-coefficients N coefficients))))

(defn get-inverse-shuffle-transformation
  [deck-size instructions]
  (->> instructions
       (reverse)
       (reduce (fn [coefficients instruction]
                 (perform-inverse-instruction-to-function-coefficients deck-size coefficients instruction))
               [1 0])
       (map (fn [c] (mod c deck-size)))))

(deftest puzzle-a-inverse-with-linear-functions
  (is= (let [position 6417
             [a b] (get-inverse-shuffle-transformation deck-size-a puzzle-input)]
         (mod (+ (* a position) b) deck-size-a))
       2019))

;; Part 2

;; Here we introduce that the shuffle occurs multiple times using geometric series

(def deck-size-b 119315717514047)
(def instructions-repeat-count-b 101741582076661)

;; a(a(a(ax + b) + b) + b) + b = a(a(a^2x + ab + b) + b) + b
;;                             = a(a^3x + a^2b + ab + b) + b
;;                             = a^4x + a^3b + a^2b + ab + b
;;                             = a^4x + b (a^4 - 1) * (a - 1)^{-1}

(defn mod-pow
  {:test (fn []
           (is= (mod-pow 2 3 7) 1))}
  [a m p]
  (.modPow (biginteger a) (biginteger m) (biginteger p)))

(deftest puzzle-a-inverse-with-repeat
  (is= (let [[a b] (get-inverse-shuffle-transformation deck-size-a puzzle-input)
             a-power-repeat-count (mod-pow a 1 deck-size-a)]
         (mod (+ (* a-power-repeat-count 6417) (* b (dec a-power-repeat-count) (mod-inverse (dec a) deck-size-a))) deck-size-a))
       2019))

(deftest puzzle-b
  (time (is= (let [[a b] (get-inverse-shuffle-transformation deck-size-b puzzle-input)
                   a-power-repeat-count (mod-pow a instructions-repeat-count-b deck-size-b)]
               (mod (+ (* a-power-repeat-count 2020) (* b (dec a-power-repeat-count) (mod-inverse (dec a) deck-size-b))) deck-size-b))
             ; "Elapsed time: 2.246417 msecs"
             98461321956136)))



