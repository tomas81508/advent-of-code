(ns advent-of-code.dec-2021.day-15
  (:require [ysera.test :refer [is= deftest]]))

(def test-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_15_input.txt"))

(defn create-atlas
  [input]
  (->> input
       (clojure.string/split-lines)
       (map-indexed (fn [y row] [y row]))
       (reduce (fn [a [y row]]
                 (->> (seq row)
                      (map-indexed (fn [x v] [x v]))
                      (reduce (fn [a [x v]]
                                (assoc a [x y] (read-string (str v))))
                              a)))
               {})))

(def test-atlas (create-atlas test-input))

(def puzzle-atlas (create-atlas puzzle-input))

(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn get-cost [atlas [x y]]
  (get atlas [x y]))

(defn on-board? [atlas coordinate]
  (contains? atlas coordinate))

(defn get-size
  [atlas]
  (->> atlas
       (keys)
       (map first)
       (apply max)
       (inc)))

(defn walk
  {:test (fn []
           (is= (walk test-atlas {:active-paths {[0 0] {:visited #{} :cost 0}}
                                  :old-paths    {}
                                  :best-path    nil})
                {:old-paths    {[0 0] {:visited #{} :cost 0}}
                 :active-paths {[1 0] {:visited #{[0 0]} :cost 1}
                                [0 1] {:visited #{[0 0]} :cost 1}}
                 :best-path    nil})
           (is= (walk test-atlas
                      {:old-paths    {[0 0] {:visited #{} :cost 0}}
                       :active-paths {[1 0] {:visited #{[0 0]} :cost 1}
                                      [0 1] {:visited #{[0 0]} :cost 1}}
                       :best-path    nil})
                {:old-paths    {[0 0] {:visited #{} :cost 0}
                                [1 0] {:visited #{[0 0]} :cost 1}
                                [0 1] {:visited #{[0 0]} :cost 1}}
                 :active-paths {[2 0] {:visited #{[0 0] [1 0]} :cost 7}
                                [1 1] {:visited #{[0 0] [1 0]} :cost 4}
                                [0 2] {:visited #{[0 0] [0 1]} :cost 3}}
                 :best-path    nil})
           (is= (walk test-atlas
                      {:old-paths    {[0 0] {:visited #{} :cost 0}
                                      [1 0] {:visited #{[0 0]} :cost 1}
                                      [0 1] {:visited #{[0 0]} :cost 1}}
                       :active-paths {[2 0] {:visited #{[0 0] [1 0]} :cost 7}
                                      [1 1] {:visited #{[0 0] [1 0]} :cost 4}
                                      [0 2] {:visited #{[0 0] [0 1]} :cost 3}}
                       :best-path    nil})
                {:old-paths    {[0 0] {:visited #{}, :cost 0}
                                [1 0] {:visited #{[0 0]}, :cost 1},
                                [1 1] {:visited #{[0 0] [1 0]}, :cost 4},
                                [0 2] {:visited #{[0 0] [0 1]}, :cost 3}
                                [0 1] {:visited #{[0 0]}, :cost 1}
                                [2 0] {:visited #{[0 0] [1 0]}, :cost 7}}
                 :active-paths {[3 0] {:visited #{[0 0] [1 0] [2 0]}, :cost 10}
                                [0 3] {:visited #{[0 0] [0 2] [0 1]}, :cost 6}
                                [2 1] {:visited #{[0 0] [1 0] [1 1]}, :cost 12}
                                [1 2] {:visited #{[0 0] [0 2] [0 1]}, :cost 4}}
                 :best-path    nil}))}
  [atlas state]
  (let [size (get-size atlas)
        goal [(dec size) (dec size)]]
    (reduce-kv (fn [state current {visited :visited cost :cost :as data}]
                 (let [state (assoc-in state [:old-paths current] data)]
                   (->> directions
                        (reduce (fn [state d]
                                  (let [p (mapv + d current)]
                                    (if-not (on-board? atlas p)
                                      state
                                      (let [cost (+ cost (get-cost atlas p))]
                                        (if (= p goal)
                                          (if (or (not (get-in state [:best-path :cost]))
                                                  (< cost (get-in state [:best-path :cost])))
                                            (assoc state :best-path {:visited (conj visited current)
                                                                     :cost    cost})
                                            state)
                                          (if (and (not (contains? visited p))
                                                   (or (not (contains? (:active-paths state) p))
                                                       (< cost (get-in state [:active-paths p :cost])))
                                                   (or (not (contains? (:old-paths state) p))
                                                       (< cost (get-in state [:old-paths p :cost]))))
                                            (assoc-in state [:active-paths p]
                                                      {:visited (conj visited current)
                                                       :cost    cost})
                                            state))))))
                                state))))
               (assoc state :active-paths {})
               (:active-paths state))))

(defn solver-a
  {:test (fn []
           (is= (solver-a test-atlas) 40))}
  [atlas]
  (loop [state {:active-paths {[0 0] {:visited #{} :cost 0}}
                :old-paths    {}
                :best-path    nil}]
    (if (empty? (:active-paths state))
      (get-in state [:best-path :cost])
      (recur (walk atlas state)))))

(comment
  (time (solver-a puzzle-atlas))
  ; "Elapsed time: 121.555835 msecs"
  790
  )

(def times [[8 9 1 2 3]
            [9 1 2 3 4]
            [1 2 3 4 5]
            [2 3 4 5 6]
            [3 4 5 6 7]])

(defn create-large-atlas
  [atlas]
  (let [size (get-size atlas)]
    (->> times
         (reduce-kv (fn [a my row]
                      (->> row
                           (reduce-kv (fn [a mx m]
                                        (let [adds (+ mx my)]
                                          (reduce-kv (fn [a [x y] v]
                                                       (let [new-x (+ (* mx size) x)
                                                             new-y (+ (* my size) y)
                                                             nv (+ adds v)]
                                                         (assoc a
                                                           [new-x new-y]
                                                           (if (> nv 9) (- nv 9) nv))))
                                                     a
                                                     atlas)))
                                      a)))
                    {}))))

(def large-test-atlas (create-large-atlas test-atlas))
(def large-puzzle-atlas (create-large-atlas puzzle-atlas))

(comment
  (time (solver-a large-test-atlas))
  ; "Elapsed time: 92.883041 msecs"
  315

  (time (solver-a large-puzzle-atlas))
  ; "Elapsed time: 103728.557388 msecs"
  2998
  )
