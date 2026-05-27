(ns advent-of-code.dec-2018.day-25
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set]))

(def test-input-1 ["-1,2,2,0" "0,0,2,-2" "0,0,0,-2" "-1,2,0,0" "-2,-2,-2,2" "3,0,2,-1" "-1,3,2,2" "-1,0,-1,0" "0,2,1,-2" "3,0,0,0"])
(def test-input-2 ["1,-1,0,1" "2,0,-1,0" "3,2,-1,0" "0,0,3,1" "0,0,-1,-1" "2,3,-2,0" "-2,2,0,0" "2,-2,0,-1" "1,-1,0,-1" "3,2,0,2"])
(def test-input-3 ["1,-1,-1,-2" "-2,-2,0,1" "0,2,1,3" "-2,3,-2,1" "0,2,3,-2" "-1,-1,1,-2" "0,-2,-1,0" "-2,2,3,-1" "1,2,2,0" "-1,-2,0,-2"])

(def input (->> (slurp "src/advent_of_code/dec_2018/day_25.txt")
                (string/split-lines)))

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input-1)
                [[-1 2 2 0] [0 0 2 -2] [0 0 0 -2] [-1 2 0 0] [-2 -2 -2 2] [3 0 2 -1] [-1 3 2 2] [-1 0 -1 0] [0 2 1 -2] [3 0 0 0]]))}
  [input]
  (->> input
       (map (fn [row] (mapv edn/read-string (string/split row #","))))))

(def test-data-1 (parse-input test-input-1))
(def test-data-2 (parse-input test-input-2))
(def test-data-3 (parse-input test-input-3))

(def data (parse-input input))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [3 1 6] [2 3 -1]) 10))}
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (reduce +)))

(defn in-range?
  {:test (fn []
           (is (in-range? #{[0 0] [1 1]} [2 2]))
           (is-not (in-range? #{[0 0] [1 1]} [3 3])))}
  [constellation point]
  (->> constellation
       (some (fn [cp] (<= (manhattan-distance cp point) 3)))))

(defn constellations
  {:test (fn []
           (is= (constellations [[1 3] [0 0]])
                #{#{[1 3]} #{[0 0]}})
           (is= (constellations [[1 1] [0 0]])
                #{#{[1 1] [0 0]}})
           (is= (constellations [[1 1] [0 0] [3 3] [4 4] [2 2]])
                #{#{[2 2] [0 0] [3 3] [1 1] [4 4]}})
           (is= (count (constellations test-data-1)) 4)
           (is= (count (constellations test-data-2)) 3)
           (is= (count (constellations test-data-3)) 8))}
  [data]
  (->> data
       (reduce (fn [constellations point]
                 (let [close-constellations (->> constellations
                                                 (filter (fn [c] (in-range? c point))))
                       constellations (apply disj constellations close-constellations)]
                   (conj constellations (apply clojure.set/union #{point} close-constellations))))
               #{})))

(comment
  (time (count (constellations data)))
  )







