(ns advent-of-code.dec-2024.day-20
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.string :as string]))

(def test-input
  ["###############"
   "#...#...#.....#"
   "#.#.#.#.#.###.#"
   "#S#...#.#.#...#"
   "#######.#.#.###"
   "#######.#.#...#"
   "#######.#.###.#"
   "###..E#...#...#"
   "###.#######.###"
   "#...###...#...#"
   "#.#####.#.###.#"
   "#.#...#.#.#...#"
   "#.#.#.#.#.#.###"
   "#...#...#...###"
   "###############"])

(def input (->> (slurp "src/advent_of_code/dec_2024/day_20_input.txt")
                (string/split-lines)))

(defn get-track
  [input]
  (->> input
       (reduce-kv (fn [a y row]
                    (->> (into [] row)
                         (reduce-kv (fn [a x c]
                                      (if (not= c \#)
                                        (conj a [x y])
                                        a))
                                    a)))
                  #{})))

(def test-track (get-track test-input))
(def track (get-track input))

(defn find-character-coordinates
  [input character]
  (->> input
       (reduce-kv (fn [a y row]
                    (let [result (->> (into [] row)
                                      (reduce-kv (fn [a x c] (if (= c character) (reduced [x y]) a))
                                                 a))]
                      (if result (reduced result) nil)))
                  nil)))

(def test-start-position (find-character-coordinates test-input \S))
(def test-end-position (find-character-coordinates test-input \E))
(def start-position (find-character-coordinates input \S))
(def end-position (find-character-coordinates input \E))



; 1. Beräkna avstånd från alla punkter till målet.

(def directions [[-1 0] [1 0] [0 1] [0 -1]])

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-track [3 7])
                #{[4 7] [3 8]}))}
  [track position]
  (->> directions
       (map (fn [d] (map + d position)))
       (filter (fn [np] (contains? track np)))
       (into #{})))

(defn distance-to-end
  {:test (fn []
           (is= (distance-to-end test-track test-end-position [3 7]) 2)
           (is= (distance-to-end test-track test-end-position test-start-position) 84))}
  [track end-position position]
  (loop [current-positions #{position}
         visited #{position}
         steps 0]
    (let [next-positions (->> current-positions
                              (map (fn [cp] (walk-a-step track cp)))
                              (apply clojure.set/union)
                              (remove (fn [np] (contains? visited np)))
                              (into #{}))]
      (cond (empty? current-positions) :error
            (contains? next-positions end-position) (inc steps)
            :else (recur next-positions
                         (clojure.set/union next-positions visited)
                         (inc steps))))))

(def non-cheating-race-length (distance-to-end track end-position start-position))

(defn distances-from-end
  {:test (fn []
           (is= (get (distances-from-end test-track test-end-position) test-start-position) 84))}
  [track end-position]
  (loop [current-positions #{end-position}
         visited {end-position 0}
         steps 0]
    (if (empty? current-positions)
      visited
      (let [next-positions (->> current-positions
                                (map (fn [cp] (walk-a-step track cp)))
                                (apply clojure.set/union)
                                (remove (fn [np] (contains? visited np)))
                                (into #{}))]
        (recur next-positions
               (->> next-positions
                    (reduce (fn [visited np]
                              (assoc visited np (inc steps)))
                            visited))
               (inc steps))))))

(def test-distances-to-end (distances-from-end test-track test-end-position))
(def distances-to-end (distances-from-end track end-position))

; 1 Givet en punkt finn alla punkter [2 0] [0 2] [-2 0] [0 -2] är värdet på någon av dessa 100 mindre?

(def cheat-moves-2 [[2 0] [0 2] [-2 0] [0 -2]])

(defn how-many-cheats
  {:test (fn []
           (is= (how-many-cheats test-distances-to-end test-track cheat-moves-2 1) 44))}
  [distances-to-end track cheat-moves threshold]
  (->> track
       (map (fn [p]
              (let [current-distance (get distances-to-end p)]
                (->> cheat-moves
                     (map (fn [cm] (map + p cm)))
                     (filter (fn [cp] (when-let [cheat-distance (get distances-to-end cp)]
                                        (<= cheat-distance (- current-distance threshold 2)))))
                     ;(map (fn [cp] [p cp (- current-distance (get distances-to-end cp) 2)]))
                     (count)))))
       (reduce +)))

(comment
  (how-many-cheats distances-to-end track cheat-moves-2 100)
  )

(def cheat-moves-20
  (->> (for [y (range -20 21)
             x (range -20 21)
             :when (<= (+ (abs x) (abs y)) 20)]
         [x y])))

(comment
  (time (how-many-cheats distances-to-end track cheat-moves-20 100))
  ; 1033120 too high
  )




