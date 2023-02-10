(ns advent-of-code.dec-2022.day-15
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_15_input.txt")
                (clojure.string/split-lines)))

(def test-input ["Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
                 "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
                 "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
                 "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
                 "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
                 "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
                 "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
                 "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
                 "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
                 "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
                 "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
                 "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
                 "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
                 "Sensor at x=20, y=1: closest beacon is at x=15, y=3"])

(def pattern #"Sensor at x=([-\d]+), y=([-\d]+): closest beacon is at x=([-\d]+), y=([-\d]+)")

(defn distance
  {:test (fn []
           (is= (distance 10 2)
                8))}
  [x1 x2] (abs (- x1 x2)))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [2 18] [-2 15])
                7))}
  [p1 p2]
  (->> (map distance p1 p2)
       (apply +)))

(defn create-state
  [input]
  (->> input
       (map (fn [row]
              (let [[_ sx sy bx by] (re-find pattern row)
                    sensor [(read-string sx) (read-string sy)]
                    beacon [(read-string bx) (read-string by)]]
                {:sensor         sensor
                 :closest-beacon beacon
                 :distance       (manhattan-distance sensor beacon)})))
       (reduce (fn [a v]
                 (-> a
                     (update :sensors conj v)
                     (update :beacons conj (:closest-beacon v))))
               {:sensors []
                :beacons #{}})))

(def test-state (create-state test-input))
(def state (create-state input))

(defn get-beacons
  {:test (fn []
           (is= (get-beacons test-state)
                #{[21 22] [15 3] [-2 15] [10 16] [25 17] [2 10]}))}
  [state]
  (->> (:sensors state)
       (map :closest-beacon)
       (set)))

(defn get-coordinates-without-beacon-at-row
  [state row]
  (clojure.set/difference
    (reduce (fn [a {[x y] :sensor d :distance}]
              (let [distance-to-row (distance row y)
                    distance-left (- d distance-to-row)]
                (if (< distance-left 0)
                  a
                  (clojure.set/union a (set (range (- x distance-left) (inc (+ x distance-left))))))))
            #{}
            (:sensors state))
    (->> (get-beacons state)
         (filter (fn [p] (= (second p) row)))
         (map first)
         (set))))

(comment
  (-> (get-coordinates-without-beacon-at-row test-state 10)
      (count))

  (time (-> (get-coordinates-without-beacon-at-row state 2000000)
            (count)))
  ; "Elapsed time: 5936.521086 msecs"
  ; => 5367037
  )

(defn possible-beacon-position?
  {:test (fn []
           (is-not (possible-beacon-position? test-state [5 5]))
           (is (possible-beacon-position? test-state [14 11])))}
  [state position]
  (->> (:sensors state)
       ; If p is > all distances
       (some (fn [{s :sensor d :distance}]
               (<= (manhattan-distance position s) d)))
       (not)))

(defn puzzle-b
  "Look at positions just outside the boundary of every sensor."
  {:test (fn []
           (is= (puzzle-b test-state 20)
                [14 11]))}
  [state max-position]
  (->>
    (:sensors state)
    (some
      (fn [{s :sensor d :distance}]
        (println "Working with sensor:" s)
        (let [parts [{:direction [-1 -1] :start (map + s [(inc d) 0])}
                     {:direction [-1 1] :start (map + s [0 (- (inc d))])}
                     {:direction [1 1] :start (map + s [(- (inc d)) 0])}
                     {:direction [1 -1] :start (map + s [0 (inc d)])}]]
          (->>
            parts
            (some
              (fn [{direction :direction start :start}]
                (println "   direction:" direction)
                (loop [amplitude 1
                       current start]
                  (when (< amplitude (+ d 2))
                    (let [p (map + current direction)]
                      (when (and (<= 0 (first p) max-position)
                                 (<= 0 (second p) max-position))
                        (if (possible-beacon-position? state p)
                          p
                          (recur (inc amplitude) p))))))))))))))

(comment
  (time (puzzle-b state 4000000))
  ; "Elapsed time: 40597.917996 msecs"
  ; => (2978645 3249288)

  (+ (* 2978645 4000000) 3249288)

  )

