(ns advent-of-code.dec-2018.day-11
  (:require [advent-of-code.test :refer [is=]]))

(def grid-serial-number 6878)


(defn find-power-level
  "The rack ID is 3 + 10 = 13.
   The power level starts at 13 * 5 = 65.
   Adding the serial number produces 65 + 8 = 73.
   Multiplying by the rack ID produces 73 * 13 = 949.
   The hundreds digit of 949 is 9.
   Subtracting 5 produces 9 - 5 = 4."
  {:test (fn []
           (is= (find-power-level [3 5] 8)
                4)
           (is= (find-power-level [122, 79] 57)
                -5)
           (is= (find-power-level [217, 196] 39)
                0)
           (is= (find-power-level [101, 153] 71)
                4)
           (is= (find-power-level [1 1] 3)
                -4))}
  [[x y] serial-number]
  (let [rack-id (+ x 10)]
    (-> rack-id
        (* y)
        (+ serial-number)
        (* rack-id)
        (str)
        (reverse)
        (nth 2)
        (str)
        (read-string)
        (- 5))))


(def find-power-level-memoized
  (memoize find-power-level))


(defn calculate-square-power-level
  {:test (fn []
           (is= (calculate-square-power-level [33 45 3] 18)
                29))}
  [[left top size] serial-number]
  (->> (for [x (range left (+ size left))
             y (range top (+ size top))]
         [x y])
       (map (fn [cell] (find-power-level-memoized cell serial-number)))
       (apply +)))


(defn find-square-a
  {:test (fn []
           (is= (find-square-a 18)
                {:top-left-coordinate [33 45]
                 :power-level         29})
           (is= (find-square-a 42)
                {:top-left-coordinate [21 61]
                 :power-level         30}))}
  [serial-number]
  (let [top-left-coordinates (for [frame-size [298]
                                   x (range 1 (inc frame-size))
                                   y (range 1 (inc frame-size))]
                               [x y])]
    (reduce (fn [result [left top]]
              (let [square-power-level (calculate-square-power-level [left top 3] serial-number)]
                (if (or (nil? (:power-level result))
                        (> square-power-level (:power-level result)))
                  {:top-left-coordinate [left top]
                   :power-level         square-power-level}
                  result)))
            {:top-left-coordinate nil
             :power-level         nil}
            top-left-coordinates)))


(defn get-summed-table
  {:test (fn []
           (is= (-> (get-summed-table 4 2)
                    (get [2 2])
                    (:summed-value))
                -13)
           (is= (-> (get-summed-table 4 2)
                    (get [1 1])
                    (:summed-value))
                -4))}
  [serial-number size]
  (reduce (fn [a [x y]]
            (assoc a [x y] {:power-level  (find-power-level [x y] serial-number)
                            :summed-value (+ (if (= y 1) 0 (:summed-value (a [x (dec y)])))
                                             (reduce (fn [sum x]
                                                       (+ sum (or (:power-level (a [x y]))
                                                                  (find-power-level [x y] serial-number))))
                                                     0
                                                     (range 1 (inc x))))}))
          {}
          (for [x (range 1 (inc size))
                y (range 1 (inc size))]
            [x y])))

(defonce summed-table-18 (get-summed-table 18 300))
(defonce summed-table-42 (get-summed-table 42 300))

(get summed-table-18 [33 46])

(defn get-power-level-square
  {:test (fn []
           (is= (get-power-level-square summed-table-18 [1 1] 1)
                -2)
           (is= (get-power-level-square summed-table-18 [33 45] 3)
                29)
           (is= (get-power-level-square summed-table-18 [90 269] 16)
                113)
           (is= (get-power-level-square summed-table-42 [232 251] 12)
                119))}
  [summed-table [left top] size]
  (+ (- (if-let [n (get summed-table [(dec left) (dec top)])]
          (:summed-value n) 0)
        (if-let [n (get summed-table [(+ (dec left) size) (dec top)])]
          (:summed-value n) 0))
     (- (:summed-value (get summed-table [(+ (dec left) size) (+ (dec top) size)]))
        (if-let [n (get summed-table [(dec left) (+ (dec top) size)])]
          (:summed-value n) 0))))


(defn get-max-power-level-square
  {:test (fn []
           (is= (get-max-power-level-square summed-table-18)
                {:best-result 113
                 :cell        [90 269]
                 :size        16}))}
  [summed-table]
  (let [size (->> (keys summed-table)
                  (map first)
                  (apply max))]
    (reduce (fn [a [x y square-size]]
              (if (nil? a)
                {:best-result (get-power-level-square summed-table [x y] square-size)
                 :cell        [x y]
                 :size        square-size}
                (let [power-level-square (get-power-level-square summed-table [x y] square-size)]
                  (if (> power-level-square (:best-result a))
                    {:best-result power-level-square
                     :cell        [x y]
                     :size        square-size}
                    a))))
            nil
            (for [x (range 1 (inc size))
                  y (range 1 (inc size))
                  square-size (range 1 (inc (inc (- size (max x y)))))]
              [x y square-size]))))


(comment
  (is= (find-square-a 6878)
       {:top-left-coordinate [20 34]
        :power-level         30}))


(comment
  (def summed-table-6878 (get-summed-table 6878 300))
  (is= (get-max-power-level-square summed-table-6878)
       {:best-result 85, :cell [90 57], :size 15}))
