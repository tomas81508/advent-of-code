(ns advent-of-code.dec-2018.day-20
  (:require [advent-of-code.test :refer [is=]]
            [clojure.core]
            [clojure.edn :as edn]
            [clojure.set :refer [union]]
            [clojure.string :as string]))

(def test-input-0 "WNE")                                    ; 3

(def test-input-1 "ENWWW(NEEE|SSE(EE|N))")                  ; 10

(def test-input-2 "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN") ; 18

(def test-input-3 "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))") ; 23

(def test-input-4 "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))") ; 31

(def input (let [s (slurp "src/advent_of_code/dec_2018/day_20.txt")]
             (subs s 1 (- (count s) 1))))

(defn parse-regex
  {:test (fn []
           (is= (parse-regex test-input-1)
                [:e :n :w :w :w [[:n :e :e :e] [:s :s :e [[:e :e] [:n]]]]]))}
  [input]
  (as-> (str "[" input "]") $
        (string/lower-case $)
        (string/replace $ "(" "[[")
        (string/replace $ "|" "][")
        (string/replace $ ")" "]]")
        (string/replace $ #"\w" ":$0 ")
        (edn/read-string $)))

(def initial-state {:cells {[0 0] 0}
                    :doors #{}})

(def to-coordinates {:e [1 0] :w [-1 0] :n [0 -1] :s [0 1]})
(def to-double-coordinates {:e [2 0] :w [-2 0] :n [0 -2] :s [0 2]})

(defn walk-a-simple-step
  {:test (fn []
           (is= (walk-a-simple-step initial-state :e [0 0])
                [{:cells {[0 0] 0 [2 0] 1}
                  :doors #{[1 0]}}
                 [2 0]]))}
  [{cells :cells :as state} direction position]
  (let [steps (inc (get cells position))
        new-position (map + position (to-double-coordinates direction))
        door (map + position (to-coordinates direction))]
    [(-> state
         (update-in [:cells new-position] (fn [old-steps] (if (nil? old-steps) steps (min old-steps steps))))
         (update :doors conj door))
     new-position]))

(defn walk-a-complex-step
  {:test (fn []
           ; simple walk
           (is= (walk-a-complex-step initial-state #{[0 0]} :e)
                [{:cells {[0 0] 0 [2 0] 1}
                  :doors #{[1 0]}}
                 #{[2 0]}])
           ; walk from multiple positions
           (is= (walk-a-complex-step {:cells {[0 0] 0 [2 0] 1} :doors #{[1 0]}} #{[2 0] [0 0]} :s)
                [{:cells {[0 0] 0 [2 0] 1 [0 2] 1 [2 2] 2}
                  :doors #{[1 0] [0 1] [2 1]}}
                 #{[0 2] [2 2]}])
           ;;; simple branching
           (is= (walk-a-complex-step initial-state #{[0 0]} [[:e :e]])
                [{:cells {[0 0] 0 [2 0] 1 [4 0] 2}
                  :doors #{[1 0] [3 0]}}
                 #{[4 0]}])
           ;;; multiple branching
           (is= (walk-a-complex-step initial-state #{[0 0]} [[:e :e] [:s]])
                [{:cells {[0 0] 0 [2 0] 1 [4 0] 2 [0 2] 1}
                  :doors #{[1 0] [3 0] [0 1]}}
                 #{[4 0] [0 2]}])
           )}
  [state positions directions]
  (if (coll? directions)
    (->> directions
         (reduce (fn [[state new-positions :as a] branch-ds]
                   (->> branch-ds
                        (reduce (fn [[state positions _] d]
                                  (let [[state new-positions] (walk-a-complex-step state positions d)]
                                    [state new-positions new-positions]))
                                [state positions new-positions])
                        ((fn [[state _ new-positions]]
                           [state (union (second a) new-positions)]))))
                 [state #{}]))
    (let [[state new-positions] (->> positions
                                     (reduce (fn [[state new-positions] p]
                                               (let [[state new-position] (walk-a-simple-step state directions p)]
                                                 [state (conj new-positions new-position)]))
                                             [state #{}]))]
      [state new-positions])))


(defn walk
  {:test (fn []
           (is= (->> (walk (parse-regex test-input-1))
                     (first)
                     (:cells)
                     (vals)
                     (reduce max))
                10))}
  [input]
  (->> input
       (reduce (fn [[state positions] v] (walk-a-complex-step state positions v))
               [initial-state #{[0 0]}])))

(defn puzzle-1
  {:test (fn []
           (is= (puzzle-1 test-input-1) 10)
           (is= (puzzle-1 test-input-2) 18)
           (is= (puzzle-1 test-input-3) 23)
           (is= (puzzle-1 test-input-4) 31))}
  [input]
  (->> (walk (parse-regex input))
       (first)
       (:cells)
       (vals)
       (reduce max)))

(comment
  (time (puzzle-1 input))
  )

(defn puzzle-2
  [input]
  (let [state (walk (parse-regex input))]
    (->> (vals (:cells (first state)))
         (remove (fn [x] (< x 1000)))
         (count))))

(comment
  (time (puzzle-2 input))
  )


















;
;(defn walk
;  {:test (fn []
;           (is= (walk [0 0] \W)
;                [-2 0]))}
;  [from direction]
;  (map + from (case direction \W [-2 0] \S [0 -2] \E [2 0] \N [0 2])))
;
;(defn midpoint
;  {:test (fn []
;           (is= (midpoint [2 0] [0 0]) [1 0])
;           (is= (midpoint [4 2] [2 2]) [3 2]))}
;  [p1 p2]
;  (->> (map + p1 p2)
;       (map (fn [x] (/ x 2)))))
;
;(defn walk-along-path
;  {:test (fn []
;           (is= (walk-along-path #{} #{} [0 0] "WNE")
;                {:cells   #{[-2 2] [0 2] [-2 0]}
;                 :doors   #{[-1 0] [-1 2] [-2 1]}
;                 :current [0 2]}))}
;  [cells doors current path]
;  (let [direction (first path)]
;    (if-not direction
;      {:cells cells :doors doors :current current}
;      (let [new-current (walk current direction)]
;        (recur (conj cells new-current)
;               (conj doors (midpoint current new-current))
;               new-current
;               (rest path))))))
;
;(defn parse-regex
;  [input]
;  (as-> (str "[" input "]") $
;        (string/replace $ "(" "[[")
;        (string/replace $ "|" "][")
;        (string/replace $ ")" "]]")
;        (edn/read-string $)))
;
;(defn walk
;  {:test (fn []
;           (is= (walk (parse-regex "ENWWW(NEEE|SSE(EE|N))"))
;                {:cells   #{[-2 2] [0 2] [-2 0]}
;                 :doors   #{[-1 0] [-1 2] [-2 1]}}))}
;  [directions]
;  (loop [[f & r] directions
;         current-positions [[0 0]]
;         cells #{}
;         doors #{}]
;    (let [])))
;
;
;(defn longest-walk
;  [cells doors]
;  )