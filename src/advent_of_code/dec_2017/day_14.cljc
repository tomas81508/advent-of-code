(ns advent-of-code.dec-2017.day-14
  (:require [ysera.test :refer [is=]]
            [advent-of-code.dec-2017.day-10 :refer [knot-hash]]
            [clojure.set :refer [difference union]]))

(def get-bits
  {\0 [0 0 0 0] \1 [0 0 0 1] \2 [0 0 1 0] \3 [0 0 1 1]
   \4 [0 1 0 0] \5 [0 1 0 1] \6 [0 1 1 0] \7 [0 1 1 1]
   \8 [1 0 0 0] \9 [1 0 0 1] \a [1 0 1 0] \b [1 0 1 1]
   \c [1 1 0 0] \d [1 1 0 1] \e [1 1 1 0] \f [1 1 1 1]})

(def input "oundnydw")

(def test-input "flqrgnkx")

(defn knot-hash-output
  [input row-index]
  (->> (knot-hash (str input "-" row-index))
       (seq)
       (map get-bits)
       (flatten)))

(defn grid
  [input]
  (->> (range 128)
       (map (fn [n] (knot-hash-output input n)))))

(defn squares-used
  [input]
  (->> (grid input)
       (map (fn [r] (reduce + r)))
       (reduce +)))

(comment
  (squares-used test-input)
  (squares-used input)
  )

(defn get-used-coordinates
  [input]
  (->> (grid input)
       (map-indexed (fn [index item] [index item]))
       (reduce (fn [used-coordinates [y v]]
                 (->> v
                      (map-indexed (fn [index item] [index item]))
                      (reduce (fn [used-coordinates [x v]]
                                (if (zero? v)
                                  used-coordinates
                                  (conj used-coordinates [x y])))
                              used-coordinates)))
               #{})))

(def used-coordinates (get-used-coordinates input))

(def directions [[-1 0] [1 0] [0 -1] [0 1]])

(defn calculate-groups
  [used-coordinates]
  (loop [used-coordinates used-coordinates
         current #{}
         groups 0
         boundary #{}]
    (if (empty? boundary)
      (if (not (empty? current))
        (recur used-coordinates
               #{}
               (inc groups)
               #{})
        (if (empty? used-coordinates)
          groups
          (let [coordinate (first used-coordinates)]
            (recur (disj used-coordinates coordinate)
                   #{coordinate}
                   groups
                   #{coordinate}))))
      ; boundary exist
      (let [extended-boundary (->> boundary
                                   (map (fn [b] (->> directions
                                                     (map (fn [d] (map + b d))))))
                                   (apply concat)
                                   (filter (fn [c] (contains? used-coordinates c)))
                                   (into #{}))]
        (recur (difference used-coordinates extended-boundary)
               (union current extended-boundary)
               groups
               extended-boundary)))))

(comment
  (calculate-groups used-coordinates)
  )

