(ns advent-of-code.dec-2017.day-21
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math]))

(def input (slurp "src/advent_of_code/dec_2017/day_21_input.txt"))

(defn flip [pattern]
  (->> pattern
       (mapv (fn [row] (apply str (reverse row))))))

(defn rotate
  {:test (fn []
           (is= (rotate [".." "##"]) [".#" ".#"])
           (is= (rotate [".#." "..#" "###"]) [".##" "#.#" "..#"])
           (is= (rotate ["..#" "#.#" ".##"]) ["###" "..#" ".#."]))}
  [pattern]
  (let [size (count pattern)]
    (->> (for [i (range size) j (range size)] [i j])
         (reduce (fn [a [i j]]
                   (assoc-in a [i j] (get-in pattern [j (- (dec size) i)])))
                 (into [] (repeat size (into [] (repeat size nil)))))
         (mapv (partial apply str)))))

(defn configurations
  {:test (fn []
           (is= (configurations ["#." ".."])
                                #{["#." ".."] [".#" ".."] [".." "#."] [".." ".#"]})
           (is= (configurations [".#." "..#" "###"])
                #{["..#" "#.#" ".##"] ["###" "..#" ".#."] ["###" "#.." ".#."] [".#." "..#" "###"]
                  [".#." "#.." "###"] ["##." "#.#" "#.."] [".##" "#.#" "..#"] ["#.." "#.#" "##."]}))}
  [pattern]
  (set ((juxt identity rotate (comp rotate rotate) (comp rotate rotate rotate)
              flip (comp flip rotate) (comp flip rotate rotate) (comp flip rotate rotate rotate))
        pattern)))

(def enhancement-rules (->> input
                            (clojure.string/split-lines)
                            (map (fn [rule] (->> (clojure.string/split rule #" => ")
                                                 (map (fn [x] (clojure.string/split x #"/"))))))
                            (reduce (fn [a [pattern value]]
                                      (let [patterns (configurations pattern)]
                                        (->> patterns
                                             (reduce (fn [a pattern]
                                                       (assoc a pattern value))
                                                     a))))
                                    {})))

(defn merge-art
  {:test (fn []
           (is= (merge-art ["#.." "#.#" ".#."] ["##." "##." "..."] ["##." "##." "..."] ["#.." "#.#" ".#."])
                ["#..##."
                 "#.###."
                 ".#...."
                 "##.#.."
                 "##.#.#"
                 "....#."]))}
  [& pieces]
  (let [n (int (clojure.math/sqrt (count pieces)))]
    (->> pieces
         (partition n)
         (map (fn [row] (apply mapv str row)))
         (flatten)
         (into []))))

(defn create-sub-art
  {:test (fn []
           (is= (create-sub-art ["...#"
                                 "#.##"
                                 "#.##"
                                 "#..."]
                                [0 0])
                [".." "#."])
           (is= (create-sub-art ["...#" "#.##" "#.##" "#..."] [0 1])
                ["#." "#."])
           (is= (create-sub-art ["...#" "#.##" "#.##" "#..."] [1 0])
                [".#" "##"])
           (is= (create-sub-art ["...#" "#.##" "#.##" "#..."] [1 1])
                ["##" ".."])
           (is= (create-sub-art [".#." "..#" "###"] [0 0])
                [".#." "..#" "###"]))}
  [art [x y]]
  (let [size (if (zero? (rem (count art) 2)) 2 3)
        where (* y size)
        start (* x size)]
    (reduce (fn [a v]
              (conj a (subs (get art (+ v where)) start (+ start size))))
            []
            (range size))))

(defn split-art
  {:test (fn []
           (is= (split-art [".#." "..#" "###"])
                [[".#." "..#" "###"]])
           (is= (split-art ["...#" "#.##" "#.##" "#..."])
                [[".." "#."] [".#" "##"] ["#." "#."] ["##" ".."]]))}
  [art]
  (let [break-up-size (if (zero? (rem (count art) 2)) 2 3)
        indexes (range (/ (count art) break-up-size))]
    (->> (for [x indexes y indexes] [x y])
         (reduce (fn [a [x y]] (assoc-in a [y x] (create-sub-art art [x y])))
                 (into [] (repeat (count indexes) (into [] (repeat (count indexes) nil)))))
         (apply concat))))

(defn increment
  {:test (fn []
           (is= (increment [".#." "..#" "###"])
                ["##.#" ".##." "#.##" "####"])
           (is= (increment ["...#" "#.##" "#.##" "#..."])
                ["####.." "##.#.#" ".#..#." "..#..#" "..#..#" "##.##."]))}
  [art]
  (->> (split-art art)
       (map enhancement-rules)
       (apply merge-art)))

(defn count-pixels
  {:test (fn []
           (is= (count-pixels [".#." "..#" "###"]) 5))}
  [art]
  (->> (reduce str art)
       (filter (fn [x] (= x \#)))
       (count)))

; part 1

(def art [".#." "..#" "###"])

(comment
  (time (count-pixels ((apply comp (repeat 5 increment)) art)))
  ; "Elapsed time: 1.063864 msecs"
  ; => 162

  (time (count-pixels ((apply comp (repeat 18 increment)) art)))
  ; "Elapsed time: 5419.798805 msecs"
  ; => 2264586
  )

















