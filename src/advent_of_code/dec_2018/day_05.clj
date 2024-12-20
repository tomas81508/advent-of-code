(ns advent-of-code.dec-2018.day-05
  (:require [advent-of-code.test :refer [is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :refer [upper-case]]))

(defn get-puzzle-input []
  (slurp "src/advent_of_code/dec_2018/day_05.txt"))

(defn polymer?
  {:test (fn []
           (is= (polymer? "c" "C") true)
           (is= (polymer? "C" "c") true)
           (is= (polymer? \C \c) true)
           (is= (polymer? "c" "c") false)
           (is= (polymer? \c \c) false)
           (is= (polymer? "C" "C") false))}
  [f s]
  (and (= (upper-case f) (upper-case s))
       (not= f s)))

(defn fully-react
  {:test (fn []
           (is= (fully-react "aA") "")
           (is= (fully-react "dabAcCaCBAcCcaDA") "dabCBAcaDA")
           (is= (fully-react "dabAcCaCBAcCcaDA" "A") "dbCBcD"))}
  ([p] (fully-react p nil))
  ([p skipped-letter]
   (let [upper-case-skipped-letter (and skipped-letter
                                        (upper-case skipped-letter))]
     (loop [coll p
            answer (list)]
       (let [f (first coll)
             tail (rest coll)]
         (cond (empty? coll)
               (apply str (reverse answer))

               (and skipped-letter
                    (= (upper-case f) upper-case-skipped-letter))
               (recur tail answer)

               (empty? answer)
               (recur tail (conj answer f))

               (polymer? f (first answer))
               (recur tail (drop 1 answer))

               :else
               (recur tail (conj answer f))))))))

(deftest puzzle-part-1
         (is= (count (fully-react (get-puzzle-input)))
              10878))

(defn minimize-fully-react
  {:test (fn []
           (is= (minimize-fully-react "dabAcCaCBAcCcaDA") "daDA"))}
  [p]
  (->> (range (int \a) (inc (int \z)))
       (map char)
       (map (partial fully-react p))
       (reduce (fn [a v]
                 (if (> (count a) (count v))
                   v
                   a)))))

(deftest puzzle-part-2
         (time (is= (count (minimize-fully-react (get-puzzle-input)))
                    6874)))
