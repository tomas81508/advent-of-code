(ns advent-of-code.dec-2017.day-11
  (:require [ysera.test :refer [is=]]))

(def input (-> (slurp "src/advent_of_code/dec_2017/day_11_input.txt")
               (clojure.string/split #",")))

(def origin [0 0])

(def directions {"n"  [0 2]
                 "nw" [-1 1]
                 "ne" [1 1]
                 "sw" [-1 -1]
                 "s"  [0 -2]
                 "se" [1 -1]})

(defn distance
  {:test (fn []
           (is= (distance [3 3] origin) 3)
           (is= (distance [0 0] origin) 0)
           (is= (distance [2 -2] origin) 2)
           (is= (distance [-1 -5] origin) 3))}
  [n1 n2]
  (/ (->> (map - n1 n2)
          (map abs)
          (reduce +))
     2))

(defn walk
  {:test (fn []
           (is= (walk ["ne" "ne" "ne"]) {:current [3 3], :max 3})
           (is= (walk ["ne" "ne" "sw" "sw"]) {:current [0 0], :max 2})
           (is= (walk ["ne" "ne", "s", "s"]) {:current [2 -2], :max 2})
           (is= (walk ["se" "sw", "se", "sw" "sw"]) {:current [-1 -5], :max 3})
           )}
  [nodes]
  (->> nodes
       (reduce (fn [a n]
                 (let [direction (directions n)
                       new-current (map + (:current a) direction)
                       distance-to-origin (distance origin new-current)]
                   (-> (if (> distance-to-origin (:max a))
                         (assoc a :max distance-to-origin)
                         a)
                       (assoc :current new-current))))
               {:current origin
                :max     0})))

(comment
  (def result (walk input))

  ; puzzle a
  (distance origin (:current result))
  722

  ; puzzle b
  (:max result)
  )


