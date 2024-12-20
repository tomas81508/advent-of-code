(ns advent-of-code.dec-2024.day-16
  (:require [clojure.string :refer [split-lines]]
            [advent-of-code.test :refer [is is-not is=]]))

(def input (split-lines (slurp "src/advent_of_code/dec_2024/day_16_input.txt")))

(def test-input-1 ["###############"
                   "#.......#....E#"
                   "#.#.###.#.###.#"
                   "#.....#.#...#.#"
                   "#.###.#####.#.#"
                   "#.#.#.......#.#"
                   "#.#.#####.###.#"
                   "#...........#.#"
                   "###.#.#####.#.#"
                   "#...#.....#.#.#"
                   "#.#.#.###.#.#.#"
                   "#.....#...#.#.#"
                   "#.###.#.#.#.#.#"
                   "#S..#.....#...#"
                   "###############"])

(defn get-walls
  [input]
  (->> input
       (reduce-kv (fn [walls y row]
                    (->> (into [] row)
                         (reduce-kv (fn [walls x c]
                                      (if (= c \#) (conj walls [x y]) walls))
                                    walls)))
                  #{})))

(def test-walls-1 (get-walls test-input-1))
(def walls (get-walls input))

(defn get-position-for-character
  [input character]
  (->> input
       (map-indexed (fn [y row] [y row]))
       (some (fn [[y row]]
               (->> row
                    (map-indexed (fn [x c] [x c]))
                    (some (fn [[x c]] (when (= c character) [x y]))))))))

(defn create-initial-state
  [input]
  (let [start-position (get-position-for-character input \S)
        end-position (get-position-for-character input \E)]
    {:visited  {start-position {[1 0] 0}}
     :boundary [{:p start-position :d [1 0] :cost 0}]
     :end      end-position}))

(def state (create-initial-state input))
(def test-state-1 (create-initial-state test-input-1))

(def turn-left {[1 0] [0 -1], [0 -1] [-1 0], [-1 0] [0 1], [0 1] [1 0]})
(def turn-right {[1 0] [0 1], [0 1] [-1 0], [-1 0] [0 -1], [0 -1] [1 0]})

(defn get-directions
  {:test (fn []
           (is= (into #{} (get-directions test-walls-1 [1 13] [1 0]))
                #{[1 0] [0 -1]}))}
  [walls p d]
  (let [naive-directions [d (turn-left d) (turn-right d)]]
    (->> naive-directions
         (remove (fn [d] (contains? walls (map + p d)))))))

(defn get-next-positions
  {:test (fn []
           (is= (->> (get-next-positions test-walls-1 [1 13] [1 0])
                     (into #{}))
                #{{:p [2 13] :d [1 0] :cost 1}
                  {:p [1 13] :d [0 -1] :cost 1000}}))}
  [walls p d]
  (let [directions (get-directions walls p d)]
    (->> directions
         (map (fn [nd] {:p (if (= nd d) (map + nd p) p) :d nd :cost (if (= nd d) 1 1000)})))))

(defn deja-vu?
  {:test (fn []
           (is (deja-vu? {[0 0] {[1 0] 4}} {:p [0 0] :d [1 0] :cost 10}))
           (is-not (deja-vu? {} {:p [0 0] :d [1 0] :cost 10}))
           (is-not (deja-vu? {[0 0] {[1 0] 4}} {:p [0 0] :d [1 0] :cost 1})))}
  [visited {p :p d :d cost :cost}]
  (let [previous-cost (get-in visited [p d])]
    (and previous-cost
         (<= previous-cost cost))))

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-walls-1 test-state-1)
                {:visited  {[1 13] {[1 0] 0 [0 -1] 1000}
                            [2 13] {[1 0] 1}}
                 :boundary [{:p [2 13] :d [1 0] :cost 1}
                            {:p [1 13] :d [0 -1] :cost 1000}]
                 :end      [13 1]})
           (is= (->> test-state-1
                     (walk-a-step test-walls-1)
                     (walk-a-step test-walls-1))
                {:visited  {[1 13] {[1 0] 0, [0 -1] 1000}
                            [2 13] {[1 0] 1}
                            [3 13] {[1 0] 2}
                            [1 12] {[0 -1] 1001}}
                 :boundary [{:p [3 13] :d [1 0] :cost 2}
                            {:p [1 12] :d [0 -1] :cost 1001}]
                 :end      [13 1]}))}
  [walls state]
  (let [visited (:visited state)
        boundary (:boundary state)
        new-boundary (->> boundary
                          (map (fn [{p :p d :d cost :cost}]
                                 (let [next-positions (get-next-positions walls p d)]
                                   (->> next-positions
                                        (map (fn [np] (update np :cost + cost)))
                                        (remove (fn [np] (deja-vu? visited np)))))))
                          (flatten))]
    (-> state
        (update :visited (fn [visited] (reduce (fn [visited {p :p d :d cost :cost}]
                                                 (update-in visited
                                                            [p d]
                                                            (fn [old-cost]
                                                              (if old-cost
                                                                (min old-cost cost)
                                                                cost))))
                                               visited
                                               new-boundary)))
        (assoc :boundary new-boundary))))

(defn shortest-distance
  [state]
  (as-> state $
        (get-in $ [:visited (:end state)])
        (vals $)
        (apply min $)))

(defn walk
  {:test (fn []
           (is= (-> (walk test-walls-1 test-state-1)
                    (shortest-distance))
                7036))}
  [walls state]
  (loop [state state]
    (let [state (walk-a-step walls state)]
      (if (empty? (:boundary state))
        state
        (recur state)))))

(comment
  (time (shortest-distance (walk walls state)))
  )

; part two

(comment
  (def complete-state (walk walls state))
  )

(def test-input ["########"
                 "#.....E#"
                 "#.#.#.##"
                 "#S....##"
                 "########"])
(def test-state (create-initial-state test-input))
(def test-walls (get-walls test-input))
(def test-complete-state (walk test-walls test-state))

(def test-complete-state-1 (walk test-walls-1 test-state-1))

(defn trace-a-step
  {:test (fn []
           (is= (trace-a-step test-complete-state {:p [6 1] :d [1 0] :cost 2007})
                [{:p [5 1] :d [1 0] :cost 2006}])
           (is= (trace-a-step test-complete-state {:p [5 1] :d [1 0] :cost 2006})
                [{:p [4 1] :d [1 0] :cost 2005}
                 {:p [5 1] :d [0 -1] :cost 1006}]))}
  [state {p :p d :d cost :cost}]
  (let [turns (->> (dissoc (get-in state [:visited p]) d)
                   (filter (fn [[_ c]] (= c (- cost 1000))))
                   (map (fn [[d c]] {:p p :d d :cost c})))
        walk (let [np (map - p d)]
               (when (= (get-in state [:visited np d]) (dec cost))
                 {:p np :d d :cost (dec cost)}))]
    (if walk
      (conj turns walk)
      turns)))

(defn trace-everything
  {:test (fn []
           (is= (trace-everything test-complete-state)
                #{[4 3] [2 3] [3 3] [1 1] [5 3] [4 1] [5 2] [1 3] [5 1] [6 1] [3 1] [2 1] [1 2] [3 2]})
           (is= (count (trace-everything test-complete-state-1)) 45))}
  [complete-state]
  (let [{ds :ds c :c} (->> (get-in complete-state [:visited (:end complete-state)])
                           (reduce-kv (fn [a d c]
                                        (cond (nil? a) {:ds [d] :c c}
                                              (< c (:c a)) {:ds [d] :c c}
                                              (= c (:c a)) (update a :ds conj d)
                                              :else a))
                                      nil))
        end-positions (->> ds
                           (map (fn [d] {:p (:end complete-state) :d d :cost c})))]
    (loop [positions end-positions
           traced #{(:end complete-state)}]
      positions
      (let [new-positions (->> positions
                               (map (fn [p] (trace-a-step complete-state p)))
                               (apply concat))]
        (if (zero? (:cost (first new-positions)))
          traced
          (recur new-positions
                 (clojure.set/union traced (->> new-positions
                                                (map :p)
                                                (into #{})))))))))

(comment
  (count (trace-everything complete-state))
  )










