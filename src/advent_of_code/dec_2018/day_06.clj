(ns advent-of-code.dec-2018.day-06
  (:require [ysera.test :refer [is= is is-not deftest]]
            [ysera.collections :refer [seq-contains?]]
            [clojure.string :as string]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2018/day_06.txt")
       (string/split-lines)
       (map (fn [string-coordinate]
              (->> (string/split string-coordinate (re-pattern ", "))
                   (map read-string))))))

(def test-coordinates [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

(defn get-bounding-rectangle
  {:test (fn []
           (is= (get-bounding-rectangle test-coordinates)
                {:x-min 1
                 :x-max 8
                 :y-min 1
                 :y-max 9}))}
  [coordinates]
  {:x-min (->> coordinates
               (map first)
               (apply min))
   :x-max (->> coordinates
               (map first)
               (apply max))
   :y-min (->> coordinates
               (map second)
               (apply min))
   :y-max (->> coordinates
               (map second)
               (apply max))})

(defn abs
  {:test (fn []
           (is= (abs 0) 0)
           (is= (abs 3) 3)
           (is= (abs -4) 4))}
  [x]
  (if (pos? x)
    x
    (- x)))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [1 3] [4 2]) 4)
           (is= (manhattan-distance [-1 3] [4 -2]) 10))}
  [p-1 p-2]
  (->> (map - p-1 p-2)
       (map abs)
       (apply +)))

(defn get-bounding-rectangle-coordinates
  [coordinates]
  (let [{x-min :x-min x-max :x-max y-min :y-min y-max :y-max}
        (get-bounding-rectangle coordinates)]
    (for [x (range x-min (inc x-max))
          y (range y-min (inc y-max))]
      [x y])))

(defn get-distances-within-bounding-rectangle
  {:test (fn []
           ; {[8 8] 5, [7 6] 4, ..., [4 8] :multiple-coordinates, ...}
           (is= (get (get-distances-within-bounding-rectangle test-coordinates)
                     [1 1])
                0)
           (is= (get (get-distances-within-bounding-rectangle test-coordinates)
                     [8 6])
                :multiple-coordinates))}
  [starting-coordinates]
  (let [coordinates (get-bounding-rectangle-coordinates starting-coordinates)]
    (reduce (fn [a coordinate]
              (let [{distance :distance
                     indexes  :closest-coordinate-indexes}
                    (->> starting-coordinates
                         (map-indexed (fn [index item] [index item]))
                         (map (fn [[index starting-coordinate]]
                                [index (manhattan-distance coordinate starting-coordinate)]))
                         (reduce (fn [a [index distance]]
                                   (cond (or (nil? (:distance a))
                                             (< distance (:distance a)))
                                         {:distance                   distance
                                          :closest-coordinate-indexes [index]}

                                         (= distance (:distance a))
                                         (update a :closest-coordinate-indexes conj index)

                                         :else
                                         a))
                                 {:distance                   nil
                                  :closest-coordinate-indexes []}))]

                (assoc a coordinate (if (> (count indexes) 1)
                                      :multiple-coordinates
                                      (first indexes)))))
            {}
            coordinates)))

(defn exist-on-the-boundary?
  {:test (fn []
           (is (-> (get-distances-within-bounding-rectangle test-coordinates)
                   (exist-on-the-boundary? 0)))
           (is-not (-> (get-distances-within-bounding-rectangle test-coordinates)
                       (exist-on-the-boundary? 4))))}
  [state value]
  (let [x-min (->> (keys state)
                   (map first)
                   (apply min))
        x-max (->> (keys state)
                   (map first)
                   (apply max))
        y-min (->> (keys state)
                   (map second)
                   (apply min))
        y-max (->> (keys state)
                   (map second)
                   (apply max))]
    (reduce-kv (fn [a [x y] v]
                 (or a
                     (and (= v value)
                          (or (= x x-min) (= x x-max)
                              (= y y-min) (= y y-max)))))
               false
               state)))

(defn get-boundary-values
  {:test (fn []
           (is= (->> test-coordinates
                     (get-distances-within-bounding-rectangle)
                     (get-boundary-values))
                [0 1 2 5]))}
  [state]
  (->> (vals state)
       (distinct)
       (filter (fn [v]
                 (and (not= v :multiple-coordinates)
                      (exist-on-the-boundary? state v))))
       (sort)))

(defn find-most-frequent-value-not-on-the-boundary
  {:test (fn []
           (is= (find-most-frequent-value-not-on-the-boundary test-coordinates)
                {:value 4 :frequency 17}))}
  [coordinates]
  (let [state (get-distances-within-bounding-rectangle coordinates)
        boundary-values (get-boundary-values state)
        inner-values (->> (vals state)
                          (remove (fn [v] (seq-contains? boundary-values v))))
        competing-values (->> inner-values
                              (distinct)
                              (remove (fn [v] (= v :multiple-coordinates)))
                              (sort)
                              (reverse))]
    (reduce (fn [a v]
              (let [frequency (->> inner-values
                                   (filter (fn [x] (= x v)))
                                   (count))]
                (if (> frequency (:frequency a))
                  {:value v :frequency frequency}
                  a)))
            {:value     nil
             :frequency 0}
            competing-values)))

(comment
  ;; 9263 msecs
  (is= (time (->> (get-puzzle-input)
                  (find-most-frequent-value-not-on-the-boundary)
                  (:frequency)))
       4186))

(defn get-close-region
  {:test (fn []
           (is= (get-close-region test-coordinates 32)
                16))}
  [coordinates limit]
  (->> coordinates
       (get-bounding-rectangle-coordinates)
       (filter (fn [c]
                 (let [total-distance (->> coordinates
                                           (map (fn [s-c] (manhattan-distance c s-c)))
                                           (apply +))]
                   (< total-distance limit))))
       (count)))

(comment
  ;; 2645 msecs
  (is= (time (get-close-region (get-puzzle-input) 10000))
       45509))
