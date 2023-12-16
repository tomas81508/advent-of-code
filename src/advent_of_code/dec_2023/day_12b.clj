(ns advent-of-code.dec-2023.day-12b
  (:require [clojure.test :refer [deftest]]
            [advent-of-code.test :refer [is is= is-not]]
            [clojure.string :refer [ends-with? split split-lines]]
            [clojure.math]
            [clojure.math.combinatorics :refer [permutations]]))


(def input (->> (slurp "src/advent_of_code/dec_2023/day_12_input.txt")
                (split-lines)))

(defn contains-damaged?
  [g]
  (some (fn [c] (= \# c)) g))

(defn contains-unknown?
  [g]
  (some (fn [c] (= \? c)) g))

(defn no-damaged?
  [groups]
  (not (some contains-damaged? groups)))

(declare memoized-solve)

(defn solve
  {:test (fn []
           (is= (solve (map seq ["???" "###"]) (list 1 1 3))
                1)
           (is= (solve (map seq ["??" "??" "?##"]) (list 1 1 3))
                4)
           (is= (solve (map seq ["?#?#?#?#?#?#?#?"]) (list 1 3 1 6))
                1)
           (is= (solve (map seq ["????" "#" "#"]) (list 4 1 1))
                1)
           (is= (solve (map seq ["????" "######" "#####"]) (list 1 6 5))
                4)
           (is= (solve (map seq ["?###????????"]) (list 3 2 1))
                10))}
  [groups ns]
  (let [g (first groups)
        gr (rest groups)
        n (first ns)
        nr (rest ns)]
    (cond (and (nil? g) (nil? n)) 1

          ; only ? left
          (and (nil? n) (no-damaged? groups)) 1

          ; nil 1 or # 0
          (or (nil? g) (nil? n)) 0

          ; #? 3
          (and (< (count g) n) (contains-damaged? g)) 0

          (< (count g) n) (solve gr ns)

          (>= (count g) n)
          (let [unused (drop n g)
                m (first unused)
                gb (drop 1 unused)]
            (+
              ; First is not included
              (if (= (first g) \?)
                (memoized-solve (cons (drop 1 g) gr) ns)
                0)
              ; First in g is included
              (if (= m \#)
                0
                (if (empty? gb)
                  (memoized-solve gr nr)
                  (let [new-groups (conj gr gb)]
                    (memoized-solve new-groups nr)))))))))

(def memoized-solve (memoize solve))

(defn create-args
  {:test (fn []
           (is= (create-args ".??..??...?##. 1,1,3")
                [[[\? \?] [\? \?] [\? \# \#] [\?] [\? \?] [\? \?] [\? \# \#] [\?] [\? \?] [\? \?] [\? \# \#] [\?] [\? \?] [\? \?] [\? \# \#] [\?] [\? \?] [\? \?] [\? \# \#]]
                 (list 1 1 3 1 1 3 1 1 3 1 1 3 1 1 3)]))}
  [line]
  (let [[f s] (clojure.string/split line #" ")]
    [(map seq (re-seq #"[\#\?]+" (clojure.string/join "?" (repeat 5 f))))
     (flatten (repeat 5 (map read-string (clojure.string/split s #","))))]))

(comment
  (time (apply memoized-solve (create-args ".??..??...?##. 1,1,3")))
  )

(deftest puzzle-b
  (is= (time (->> input
                  (map create-args)
                  (map (fn [args] (apply memoized-solve args)))
                  (reduce +)))
       ; "Elapsed time: 1444.386276 msecs"
       850504257483930))

















