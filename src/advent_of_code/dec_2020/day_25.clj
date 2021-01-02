(ns advent-of-code.dec-2020.day-25
  (:require [ysera.test :refer [is is-not is= deftest]]
            [ysera.collections :refer [seq-contains?]]))

(def puzzle-input [13316116 13651422])

(defn loop-size
  {:test (fn []
           (is= (loop-size 7 5764801)
                8)
           (is= (loop-size 7 17807724)
                11))}
  [subject-number public-key]
  (loop [i 1
         v 1]
    (let [v (rem (* v subject-number) 20201227)]
      (if (= v public-key)
        i
        (recur (inc i) v)))))

(defn produce-encryption-key
  {:test (fn []
           (is= (produce-encryption-key 17807724 8)
                14897079)
           (is= (produce-encryption-key 5764801 11)
                14897079))}
  [public-key loop-size]
  (loop [i 1
         v 1]
    (let [v (rem (* v public-key) 20201227)]
      (if (= i loop-size)
        v
        (recur (inc i) v)))))

(deftest puzzle-a
         (let [door-public-key (first puzzle-input)
               card-public-key (second puzzle-input)
               door-loop-size (loop-size 7 door-public-key)
               card-loop-size (loop-size 7 card-public-key)]
           (is= (produce-encryption-key door-public-key card-loop-size)
                12929)
           (is= (produce-encryption-key card-public-key door-loop-size)
                12929)))
