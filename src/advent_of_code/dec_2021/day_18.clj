(ns advent-of-code.dec-2021.day-18
  (:require [ysera.test :refer [is= deftest]]
            [clojure.math.combinatorics :refer [combinations]]
            [ysera.math :refer [floor ceil]]))

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_18_input.txt"))

(defn lists->depths
  {:test (fn []
           (is= (lists->depths [[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]])
                [[3 1] [2 2] [1 3] [7 4] [3 4] [6 1] [5 2] [4 3] [3 4] [2 4]]))}
  ([ns] (lists->depths ns 0))
  ([ns depth] (lists->depths ns depth []))
  ([ns depth result]
   (reduce (fn [a n]
             (if (number? n)
               (conj a [n depth])
               (apply conj a (lists->depths n (inc depth)))))
           result
           ns)))

(defn snail-explode
  {:test (fn []
           (is= (-> [[[[[9 8] 1] 2] 3] 4]
                    (lists->depths)
                    (snail-explode))
                (lists->depths [[[[0 9] 2] 3] 4]))
           (is= (-> [7 [6 [5 [4 [3 2]]]]]
                    (lists->depths)
                    (snail-explode))
                (lists->depths [7 [6 [5 [7 0]]]]))
           (is= (-> [[6 [5 [4 [3 2]]]] 1]
                    (lists->depths)
                    (snail-explode))
                (lists->depths [[6 [5 [7 0]]] 3]))
           (is= (-> [[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]]
                    (lists->depths)
                    (snail-explode))
                (lists->depths [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]]))
           (is= (-> [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]]
                    (lists->depths)
                    (snail-explode))
                (lists->depths [[3 [2 [8 0]]] [9 [5 [7 0]]]]))
           (is= (snail-explode [[0 3] [7 3] [4 2] [7 3] [8 3] [0 3] [6 4] [7 4] [1 1] [1 1]])
                [[0 3] [7 3] [4 2] [7 3] [8 3] [6 3] [0 3] [8 1] [1 1]])
           )}
  [vds]
  (let [l (count vds)
        index (->> vds
                   (map-indexed (fn [index x] [index x]))
                   (some (fn [[index [_ d]]] (when (= d 4) index))))]
    (if (not index)
      vds
      (as-> vds $
            (assoc $ index nil (inc index) [0 3])
            (if (pos? index)
              (update $ (dec index) (fn [[x d]] [(+ x (get-in vds [index 0])) d]))
              $)
            (if (< index (- l 2))
              (update $ (+ index 2) (fn [[x d]] [(+ x (get-in vds [(inc index) 0])) d]))
              $)
            (remove nil? $)
            (into [] $)))))


(defn snail-split
  {:test (fn []
           (is= (-> [[[[0 7] 4] [15 [0 13]]] [1 1]]
                    (lists->depths)
                    (snail-split))
                (lists->depths [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]))
           (is= (-> [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]
                    (lists->depths)
                    (snail-split))
                (lists->depths [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]])))}
  [vds]
  (let [index (->> vds
                   (map-indexed (fn [index x] [index x]))
                   (some (fn [[index [v _]]] (when (> v 9) index))))]
    (if (not index)
      vds
      (loop [[[v d :as x] & r] vds
             result []]
        (if (> v 9)
          (->> (concat (conj result
                             [(floor (/ v 2)) (inc d)]
                             [(ceil (/ v 2)) (inc d)])
                       r)
               (into []))
          (recur r (conj result x)))))))

(defn snail-reduce
  {:test (fn []
           (is= (-> [[[[[4 3] 4] 4] [7 [[8 4] 9]]] [1 1]]
                    (lists->depths)
                    (snail-reduce))
                (lists->depths [[[[0 7] 4] [[7 8] [6 0]]] [8 1]])))}
  [vds]
  (loop [vds vds]
    (let [exploded-vds (snail-explode vds)]
      (if (not= vds exploded-vds)
        (recur exploded-vds)
        (let [split-vds (snail-split vds)]
          (if (not= vds split-vds)
            (recur split-vds)
            vds))))))


(defn snail-add
  {:test (fn []
           (is= (->> [[[[0 [4 5]] [0 0]] [[[4 5] [2 6]] [9 5]]]
                      [7 [[[3 7] [4 3]] [[6 3] [8 8]]]]
                      [[2 [[0 8] [3 4]]] [[[6 7] 1] [7 [1 6]]]]
                      [[[[2 4] 7] [6 [0 5]]] [[[6 8] [2 8]] [[2 1] [4 5]]]]
                      [7 [5 [[3 8] [1 4]]]]
                      [[2 [2 2]] [8 [8 1]]]
                      [2 9]
                      [1 [[[9 3] 9] [[9 0] [0 7]]]]
                      [[[5 [7 4]] 7] 1]
                      [[[[4 2] 2] 6] [8 7]]]
                     (map lists->depths)
                     (snail-add))
                (lists->depths [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]])))}
  [vdss]
  (let [add-two (fn [xs ys]
                  (into []
                        (concat (map (fn [x] (update x 1 inc)) xs)
                                (map (fn [y] (update y 1 inc)) ys))))]
    (reduce (fn [a v]
              (->> (add-two a v)
                   (into [])
                   (snail-reduce)))
            vdss)))

(defn depths->lists
  {:test (fn []
           (is= (depths->lists [[9 4] [8 4] [1 3] [2 2] [3 1] [4 0]])
                [[[[[9 8] 1] 2] 3] 4])
           (is= (depths->lists [[4 2] [2 2] [3 2] [5 2] [1 2] [6 2] [9 1]])
                [[[4 2] [3 5]] [[1 6] 9]])
           (is= (depths->lists [[7 0] [6 1] [5 2] [4 3] [3 4] [2 4]])
                [7 [6 [5 [4 [3 2]]]]])
           (is= (depths->lists [[8 3] [7 3] [7 3] [7 3] [8 3] [6 3] [7 3] [7 3] [0 3] [7 3] [6 3] [6 3] [8 2] [7 2]])
                [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]])
           )}
  [vds]
  (loop [vds vds
         level 4]
    (if (neg? level)
      (ffirst vds)
      (let [result (loop [vds vds
                          result []]
                     (let [[x1 d1 :as element] (first vds)
                           [x2 d2] (second vds)]
                       (cond (nil? x1)
                             result

                             (= d1 d2 level)
                             (recur (drop 2 vds)
                                    (conj result [[x1 x2] (dec level)]))

                             :else
                             (recur (drop 1 vds)
                                    (conj result element)))))]
        (recur result (dec level))))))

(defn calculate-magnitude
  {:test (fn []
           (is= (calculate-magnitude [[1 2] [[3 4] 5]]) 143)
           (is= (calculate-magnitude [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]) 1384)
           (is= (calculate-magnitude [[[[1 1] [2 2]] [3 3]] [4 4]]) 445)
           (is= (calculate-magnitude [[[[3 0] [5 3]] [4 4]] [5 5]]) 791)
           (is= (calculate-magnitude [[[[5 0] [7 4]] [5 5]] [6 6]]) 1137)
           (is= (calculate-magnitude [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]]) 3488)

           )}
  [[l r]]
  (+ (* 3 (if (number? l) l (calculate-magnitude l)))
     (* 2 (if (number? r) r (calculate-magnitude r)))))

(defn solver-a
  []
  (->> puzzle-input
       (clojure.string/split-lines)
       (map read-string)
       (map lists->depths)
       (snail-add)
       (depths->lists)
       (calculate-magnitude)))

(comment
  (time (solver-a))
  ; "Elapsed time: 104.189017 msecs"
  4173
  )

(defn largest-magnitude-duo
  {:test (fn []
           (is= (->> [[[[0 [5 8]] [[1 7] [9 6]]] [[4 [1 2]] [[1 4] 2]]]
                      [[[5 [2 8]] 4] [5 [[9 9] 0]]]
                      [6 [[[6 2] [5 6]] [[7 6] [4 7]]]]
                      [[[6 [0 7]] [0 9]] [4 [9 [9 0]]]]
                      [[[7 [6 4]] [3 [1 3]]] [[[5 5] 1] 9]]
                      [[6 [[7 3] [3 2]]] [[[3 8] [5 7]] 4]]
                      [[[[5 4] [7 7]] 8] [[8 3] 8]]
                      [[9 3] [[9 9] [6 [4 9]]]]
                      [[2 [[7 7] 7]] [[5 8] [[9 3] [0 2]]]]
                      [[[[5 2] 5] [8 [3 7]]] [[5 [7 5]] [4 4]]]]
                     (map lists->depths)
                     (largest-magnitude-duo))
                3993))}
  [vdss]
  (let [combs (combinations vdss 2)]
    (reduce (fn [a vds]
              (max a
                   (calculate-magnitude (depths->lists (snail-add vds)))
                   (calculate-magnitude (depths->lists (snail-add (reverse vds))))))
            0
            combs)))

(defn solver-b
  []
  (->> puzzle-input
       (clojure.string/split-lines)
       (map read-string)
       (map lists->depths)
       (largest-magnitude-duo)))

(comment
  (time (solver-b))
  ; "Elapsed time: 1046.621118 msecs"
  4706
  )
