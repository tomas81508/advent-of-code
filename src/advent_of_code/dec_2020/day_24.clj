(ns advent-of-code.dec-2020.day-24
  (:require [ysera.test :refer [is is-not is= deftest]]
            [ysera.collections :refer [seq-contains?]]))

(def test-input ["sesenwnenenewseeswwswswwnenewsewsw"
                 "neeenesenwnwwswnenewnwwsewnenwseswesw"
                 "seswneswswsenwwnwse"
                 "nwnwneseeswswnenewneswwnewseswneseene"
                 "swweswneswnenwsewnwneneseenw"
                 "eesenwseswswnenwswnwnwsewwnwsene"
                 "sewnenenenesenwsewnenwwwse"
                 "wenwwweseeeweswwwnwwe"
                 "wsweesenenewnwwnwsenewsenwwsesesenwne"
                 "neeswseenwwswnwswswnw"
                 "nenwswwsewswnenenewsenwsenwnesesenew"
                 "enewnwewneswsewnwswenweswnenwsenwsw"
                 "sweneswneswneneenwnewenewwneswswnese"
                 "swwesenesewenwneswnwwneseswwne"
                 "enesenwswwswneneswsenwnewswseenwsese"
                 "wnwnesenesenenwwnenwsewesewsesesew"
                 "nenewswnwewswnenesenwnesewesw"
                 "eneswnwswnwsenenwnwnwwseeswneewsenese"
                 "neswnwewnwnwseenwseesewsenwsweewe"
                 "wseweeenwnesenwwwswnew"])

(def puzzle-input (->> (slurp "src/advent_of_code/dec_2020/day_24.txt")
                       (clojure.string/split-lines)))

(defn find-coordinate
  {:test (fn []
           (is= (find-coordinate "sesenwnenenewseeswwswswwnenewsewsw")
                [-4 2]))}
  [input]
  (loop [input input
         result [0 0]]
    (let [match-result (re-matches #"(se|sw|w|nw|ne|e)(.*)" input)]
      (if (nil? match-result)
        result
        (recur (nth match-result 2)
               (map + result (case (nth match-result 1)
                               "se" [1 1]
                               "sw" [-1 1]
                               "w" [-2 0]
                               "nw" [-1 -1]
                               "ne" [1 -1]
                               "e" [2 0])))))))

(defn get-black-coordinates
  [coordinates]
  (->> coordinates
       (frequencies)
       (seq)
       (filter (fn [[_ v]] (odd? v)))
       (map first)))

(defn calculate-black-tiles
  {:test (fn []
           (is= (calculate-black-tiles [[1 0] [1 0] [2 0] [4 0] [4 0] [4 0]])
                2)
           (is= (->> test-input
                     (map find-coordinate)
                     (calculate-black-tiles))
                10))}
  [coordinates]
  (count (get-black-coordinates coordinates)))

(deftest puzzle-a
         (is= (time (->> puzzle-input
                         (map find-coordinate)
                         (calculate-black-tiles)))
              320))

(def directions #{[-2 0] [2 0] [-1 -1] [-1 1] [1 -1] [1 1]})

(defn get-neighbors
  {:test (fn []
           (is= (set (get-neighbors [2 0]))
                #{[0 0] [1 1] [3 -1] [1 -1] [3 1] [4 0]}))}
  [c]
  (map (fn [d] (map + d c)) directions))

(defn find-all-interesting-coordinates
  {:test (fn []
           (is= (find-all-interesting-coordinates [[0 0]])
                #{[-2 0] [2 0] [-1 -1] [-1 1] [1 -1] [1 1] [0 0]}))}
  [coordinates]
  (->> coordinates
       (map (fn [c]
              (conj (get-neighbors c) c)))
       (apply concat)
       (set)))

(defn abs [x] (if (neg? x) (- x) x))

(defn neighbors?
  {:test (fn []
           (is (neighbors? [0 0] [2 0]))
           (is (neighbors? [0 0] [1 1]))
           (is-not (neighbors? [0 0] [4 0]))
           (is-not (neighbors? [0 0] [2 2])))}
  [c1 c2]
  (= (->> (map - c1 c2)
          (map abs)
          (apply +))
     2))

(defn remain-black?
  {:test (fn []
           (is-not (remain-black? [0 0] []))
           (is-not (remain-black? [0 0] [[-2 0] [2 0] [1 1] [4 0]]))
           (is (remain-black? [0 0] [[2 0]])))}
  [coordinate black-coordinates]
  (let [black-neighbors (clojure.set/intersection (set (get-neighbors coordinate))
                                                  (set black-coordinates))]
    (< 0 (count black-neighbors) 3)))

(defn turn-to-black?
  {:test (fn []
           (is-not (turn-to-black? [0 0] []))
           (is (turn-to-black? [0 0] [[-2 0] [2 0]]))
           (is-not (turn-to-black? [0 0] [[2 0]])))}
  [coordinate black-coordinates]
  (= (count (clojure.set/intersection (set (get-neighbors coordinate))
                                      (set black-coordinates)))
     2))

(defn flip-tiles
  {:test (fn []
           (is= (->> test-input
                     (map find-coordinate)
                     (get-black-coordinates)
                     (into #{})
                     (flip-tiles)
                     (count))
                15)
           )}
  [coordinates]
  (->> coordinates
       (find-all-interesting-coordinates)
       (filter (fn [c]
                 (if (seq-contains? coordinates c)
                   (remain-black? c coordinates)
                   (turn-to-black? c coordinates))))))

(deftest puzzle-test
         (is= (time (as-> test-input $
                          (map find-coordinate $)
                          (get-black-coordinates $)
                          (into #{} $)
                          (loop [black-coordinates $
                                 counter 0]
                            (println counter)
                            (if (= counter 100)
                              black-coordinates
                              (recur (flip-tiles black-coordinates)
                                     (inc counter))))
                          (count $)))
              ; "Elapsed time: 108262.898953 msecs"
              2208))

(deftest puzzle-b
         (is= (time (as-> puzzle-input $
                          (map find-coordinate $)
                          (get-black-coordinates $)
                          (into #{} $)
                          (loop [black-coordinates $
                                 counter 0]
                            (println counter)
                            (if (= counter 100)
                              black-coordinates
                              (recur (flip-tiles black-coordinates)
                                     (inc counter))))
                          (count $)))
              ; "Elapsed time: 445577.407061 msecs"
              3777))










