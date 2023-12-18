(ns advent-of-code.dec-2023.day-18
  (:require [advent-of-code.test :refer [is= is is-not]]))

; solved with Daniel Gullberg, Mattias Liljeström, Emil Myresten and Anders Lång

(def input (slurp "src/advent_of_code/dec_2023/day_18_input.txt"))
(def test-input "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)")

(defn parse-line
  {:test (fn []
           (is= (parse-line "R 6 (#70c710)") ["R" 6 "70c710"]))}
  [line]
  (let [[p1 p2 p3] (clojure.string/split line #" ")]
    [p1 (read-string p2) (first (re-find #"(\d|[a-z])+" p3))]))

(defn dir->dir
  [dir]
  (condp = dir
    "R" [0 1]
    "L" [0 -1]
    "U" [-1 0]
    "D" [1 0]))

(defn fill-interior
  [dig-positions]
  ;; start position is inside for this data
  (loop [positions-to-test [[1 1]]
         interior dig-positions]
    (if (empty? positions-to-test)
      interior
      (let [[position-to-test & rest] positions-to-test
            new-positions (reduce (fn [a d]
                                    (let [new-pos (map + position-to-test d)]
                                      (if (contains? interior new-pos)
                                        a
                                        (conj a new-pos)))
                                    )
                                  #{}
                                  (map dir->dir ["R" "L" "D" "U"]))]
        (recur (concat rest (into [] new-positions)) (clojure.set/union interior new-positions))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 62))}
  [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (reduce (fn [[pos a] [dir steps _]]
                 (reduce (fn [[pos a] _]
                           (let [new-pos (map + pos (dir->dir dir))]
                             [new-pos (conj a new-pos)]))
                         [pos a]
                         (range steps)))
               [[0 0] #{}])
       (second)
       (fill-interior)
       (count)))

(defn dir->dir-b
  [dir]
  (condp = dir
    "0" [0 1]
    "2" [0 -1]
    "3" [-1 0]
    "1" [1 0]))

;; https://en.wikipedia.org/wiki/Shoelace_formula
(defn shoelace
  {:test (fn []
           (is= (shoelace [[0 0] [2 0] [2 2] [0 2] [0 0]]) 4)
           (is= (shoelace [[0 0] [0 6] [5 6] [5 4] [7 4] [7 6] [9 6] [9 1] [7 1] [7 0] [5 0] [5 2] [2 2] [2 0] [0 0]]) 42))}
  [points]
  (loop [res 0
         p1 (first points)
         p2 (second points)
         the-rest (drop 2 points)]
    (let [this-res (- (* (first p1) (second p2))
                      (* (second p1) (first p2)))
          new-res (+ res this-res)]
      (if (empty? the-rest)
        (abs (/ new-res 2))
        (recur new-res p2 (first the-rest) (drop 1 the-rest))))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 952408144115))}
  [input]
  (let [[_ points exterior-length] (->> (clojure.string/split-lines input)
                                        (map parse-line)
                                        (reduce (fn [[pos points exterior-length] [_ _ colour]]
                                                  (let [steps (Integer/parseInt (subs colour 0 5) 16)
                                                        dir (dir->dir-b (subs colour 5))
                                                        new-pos (map + pos (map * dir [steps steps]))]
                                                    [new-pos (conj points new-pos) (+ exterior-length steps)]))
                                                [[0 0] [[0 0]] 0]))]
    ;; Half of exterior area included in shoelace. half from exterior length, and + 1 for the 4 corner points
    (+ 1 (/ exterior-length 2) (shoelace points))))

(comment
  (time (solve-a input))
  ;; 66993
  ;; "Elapsed time: 2803.676581 msecs"

  (time (solve-b input))
  ;; 92291468914147
  ;; "Elapsed time: 4.459149 msecs"
  )