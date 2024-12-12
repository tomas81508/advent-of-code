(ns advent-of-code.dec-2024.day-12
  (:require [advent-of-code.test :refer [is= is is-not]]))

; Clojure meetup with Daniel Gullberg and Peter PEZ Strömberg

(def input (slurp "src/advent_of_code/dec_2024/day_12_input.txt"))

(def test-input "AAAA\nBBCD\nBBCC\nEEEC")
(def test-input-large "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE")

(defn create-garden-map
  [input]
  (let [lines (into [] (clojure.string/split-lines input))]
    (reduce-kv (fn [a i line]
                 (reduce-kv (fn [a j c]
                              (assoc a [i j] c))
                            a
                            (into [] line)))
               {}
               lines)))

(def garden-map (create-garden-map input))
(def test-garden-map (create-garden-map test-input))
(def test-garden-map-large (create-garden-map test-input-large))

(def directions [[-1 0] [1 0] [0 -1] [0 1]])

(defn find-region
  {:test (fn []
           (is= (find-region test-garden-map [0 0])
                #{[0 0] [0 1] [0 2] [0 3]})
           (is= (find-region test-garden-map [2 2])
                #{[1 2] [2 2] [2 3] [3 3]}))}
  [garden-map position]
  (let [plant (get garden-map position)]
    (loop [region #{position}]
      (let [next-region (->> region
                             (map (fn [p] (->> directions
                                               (map (fn [d] (map + d p)))
                                               (filter (fn [np] (= plant (get garden-map np))))
                                               (remove region))))
                             (apply concat)
                             (remove empty?)
                             (into #{})
                             (clojure.set/union region))]
        (if (= next-region region)
          region
          (recur next-region))))))

(defn find-regions
  {:test (fn []
           (is= (find-regions test-garden-map)
                #{#{[1 3]}
                  #{[3 0] [3 1] [3 2]}
                  #{[1 0] [1 1] [2 0] [2 1]}
                  #{[2 2] [2 3] [3 3] [1 2]}
                  #{[0 0] [0 3] [0 2] [0 1]}}))}
  [garden-map]
  (loop [garden-map garden-map
         regions #{}]
    (if (empty? garden-map)
      regions
      (let [[position _] (first garden-map)]
        (let [region (find-region garden-map position)]
          (recur (apply dissoc garden-map region)
                 (conj regions region)))))))

(defn get-perimeter
  [region]
  (->> region
       (map (fn [p] (->> directions
                         (map (fn [d] (map + d p)))
                         (remove region))))
       (apply concat)))

(defn get-perimeter-length
  {:test (fn []
           (is= (get-perimeter-length #{[1 2] [2 2] [2 3] [3 3]}) 10))}
  [region]
  (count (get-perimeter region)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-garden-map) 140)
           (is= (part-1 test-garden-map-large) 1930))}
  [garden-map]
  (let [regions (find-regions garden-map)]
    (reduce (fn [a region]
              (+ a (* (count region) (get-perimeter-length region))))
            0
            regions)))

(defn get-side
  {:test (fn []
           (is= (get-side #{[0 0] [0 1] [0 2] [0 3]} [1 0] [-1 0] [[0 1] [0 -1]])
                #{[1 0] [1 1] [1 2] [1 3]})
           (is= (get-side (find-region test-garden-map-large [5 2]) [5 3] [0 -1] [[1 0] [-1 0]])
                #{[5 3]}))}
  [region position region-direction directions-to-walk]
  (loop [side-points #{position}]
    (let [new-points (->> side-points
                          (map (fn [p] (->> directions-to-walk
                                            (map (fn [d] (map + d p)))
                                            (remove region)
                                            (filter (fn [p] (contains? region (map + p region-direction))))
                                            (remove side-points))))
                          (apply concat)
                          (into #{}))]
      (if (empty? new-points)
        side-points
        (recur (clojure.set/union side-points new-points))))))

(def other-directions {[0 1]  [[1 0] [-1 0]]
                       [0 -1] [[1 0] [-1 0]]
                       [1 0]  [[0 1] [0 -1]]
                       [-1 0] [[0 1] [0 -1]]})

(defn get-number-of-sides
  {:test (fn []
           (is= (get-number-of-sides #{[0 0] [0 1] [0 2] [0 3]}) 4)
           (is= (get-number-of-sides #{[1 2] [2 2] [2 3] [3 3]}) 8)
           (is= (get-number-of-sides (find-region test-garden-map-large [5 2]))
                16))}
  [region]
  (let [boundary-points (into #{} (get-perimeter region))
        boundary-points-with-directions (reduce (fn [a p]
                                                  (assoc a p (->> directions
                                                                  (filter (fn [d] (contains? region (map + d p))))
                                                                  (into #{}))))
                                                {}
                                                boundary-points)]
    (loop [boundary-points-with-directions boundary-points-with-directions
           sides 0]
      (if (empty? boundary-points-with-directions)
        sides
        (let [[point directions] (first boundary-points-with-directions)
              direction (first directions)
              side-points (get-side region point direction (other-directions direction))]
          (recur (reduce (fn [a p]
                           (if (< 1 (count (get a p)))
                             (update a p disj direction)
                             (dissoc a p)))
                         boundary-points-with-directions
                         side-points)
                 (inc sides)))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-garden-map) 80)
           (is= (part-2 test-garden-map-large) 1206))}
  [garden-map]
  (let [regions (find-regions garden-map)]
    (reduce (fn [a region]
              (+ a (* (count region) (get-number-of-sides region))))
            0
            regions)))

(comment
  ;; "Elapsed time: 637.255417 msecs"
  ;=> 1431316
  (time (part-1 garden-map))

  ;; "Elapsed time: 724.939291 msecs"
  ;=> 821428
  (time (part-2 garden-map))
  )



