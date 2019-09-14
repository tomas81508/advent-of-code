(ns advent-of-code.dec-2018.day-10
  (:require [ysera.test :refer [deftest is is-not is=]]))

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2018/day_10.txt")
      (clojure.string/split-lines)))


(def test-data ["position=< 9,  1> velocity=< 0,  2>"
                "position=< 7,  0> velocity=<-1,  0>"
                "position=< 3, -2> velocity=<-1,  1>"
                "position=< 6, 10> velocity=<-2, -1>"
                "position=< 2, -4> velocity=< 2,  2>"
                "position=<-6, 10> velocity=< 2, -2>"
                "position=< 1,  8> velocity=< 1, -1>"
                "position=< 1,  7> velocity=< 1,  0>"
                "position=<-3, 11> velocity=< 1, -2>"
                "position=< 7,  6> velocity=<-1, -1>"
                "position=<-2,  3> velocity=< 1,  0>"
                "position=<-4,  3> velocity=< 2,  0>"
                "position=<10, -3> velocity=<-1,  1>"
                "position=< 5, 11> velocity=< 1, -2>"
                "position=< 4,  7> velocity=< 0, -1>"
                "position=< 8, -2> velocity=< 0,  1>"
                "position=<15,  0> velocity=<-2,  0>"
                "position=< 1,  6> velocity=< 1,  0>"
                "position=< 8,  9> velocity=< 0, -1>"
                "position=< 3,  3> velocity=<-1,  1>"
                "position=< 0,  5> velocity=< 0, -1>"
                "position=<-2,  2> velocity=< 2,  0>"
                "position=< 5, -2> velocity=< 1,  2>"
                "position=< 1,  4> velocity=< 2,  1>"
                "position=<-2,  7> velocity=< 2, -2>"
                "position=< 3,  6> velocity=<-1, -1>"
                "position=< 5,  0> velocity=< 1,  0>"
                "position=<-6,  0> velocity=< 2,  0>"
                "position=< 5,  9> velocity=< 1, -2>"
                "position=<14,  7> velocity=<-2,  0>"
                "position=<-3,  6> velocity=< 2, -1>"])

(defn create-state
  {:test (fn []
           (is= (create-state ["position=< 9,  1> velocity=< 0,  2>"
                               "position=< 7,  0> velocity=<-1,  0>"
                               "position=< 3, -2> velocity=<-1,  1>"])
                {:initial-data [{:position [9 1] :velocity [0 2]}
                                {:position [7 0] :velocity [-1 0]}
                                {:position [3 -2] :velocity [-1 1]}]
                 :time         0}))}
  [data]
  (let [pattern (re-pattern "position=<([^,]+),([^>]+)> velocity=<([^,]+),([^>]+)>")]
    {:initial-data (->> data
                        (map (fn [data-line]
                               (let [[_ px py vx vy] (re-find pattern data-line)]
                                 {:position [(read-string px) (read-string py)]
                                  :velocity [(read-string vx) (read-string vy)]}))))
     :time         0}))

(defn time-tick
  {:test (fn []
           (is= (time-tick {:initial-data [{:position [9 1] :velocity [0 2]}
                                           {:position [7 0] :velocity [-1 0]}
                                           {:position [3 -2] :velocity [-1 1]}]
                            :time         0})
                {:initial-data [{:position [9 1] :velocity [0 2]}
                                {:position [7 0] :velocity [-1 0]}
                                {:position [3 -2] :velocity [-1 1]}]
                 :time         1}))}
  [state]
  (-> state
      (update :time inc)))


(defn get-positions
  {:test (fn []
           (is= (get-positions {:initial-data [{:position [9 1] :velocity [0 2]}
                                               {:position [7 0] :velocity [-1 0]}
                                               {:position [3 -2] :velocity [-1 1]}]
                                :time         2})
                [[9 5] [5 0] [1 0]]))}
  [state]
  (->> (:initial-data state)
       (map (fn [{position :position velocity :velocity}]
              (map + position (map (fn [n] (* (:time state) n)) velocity))))))


(defn get-boundary
  {:test (fn []
           (is= (-> {:initial-data [{:position [9 1] :velocity [0 2]}
                                    {:position [7 0] :velocity [-1 0]}
                                    {:position [3 -2] :velocity [-1 1]}]
                     :time         0}
                    (get-positions)
                    (get-boundary))
                [3 9 -2 1])
           (is= (-> {:initial-data [{:position [9 1] :velocity [0 2]}
                                    {:position [7 0] :velocity [-1 0]}
                                    {:position [3 -2] :velocity [-1 1]}]
                     :time         3}
                    (get-positions)
                    (get-boundary))
                [0 9 0 7]))}
  [positions]
  (reduce (fn [a [x y]]
            [(min (nth a 0) x)
             (max (nth a 1) x)
             (min (nth a 2) y)
             (max (nth a 3) y)])
          [10000 -10000 10000 -10000]
          positions))


(defn time-ticks
  [state n]
  (if (pos? n)
    (recur (time-tick state) (dec n))
    state))


(defn tick-until-smallest-shape
  {:test (fn []
           (is= (-> (create-state test-data)
                    (tick-until-smallest-shape)
                    (:time))
                3))}
  [state]
  (reduce (fn [state _]
            (when (zero? (mod (:time state) 100))
              (println "time:" (:time state)))
            (let [positions (get-positions state)
                  [xmin xmax _ _] (get-boundary positions)
                  next-state (time-tick state)
                  next-positions (get-positions next-state)
                  [next-xmin next-xmax _ _] (get-boundary next-positions)]
              (if (< (- next-xmax next-xmin) (- xmax xmin))
                next-state
                (reduced state))))
          state
          (range)))



(defn star?
  [positions position]
  (->> positions
       (filter (fn [point]
                 (= point position)))
       (first)))


(defn state->string
  {:test (fn []
           (is= (state->string {:initial-data [{:position [9 1] :velocity [0 2]}
                                               {:position [7 0] :velocity [-1 0]}
                                               {:position [3 -2] :velocity [-1 1]}]
                                :time         0})
                (str "After 0 seconds:\n"
                     "#......\n"
                     ".......\n"
                     "....#..\n"
                     "......#\n")))}
  [state]
  (let [positions (get-positions state)]
    (let [[xmin xmax ymin ymax] (get-boundary positions)]
      (apply str
             "After " (:time state) " seconds:\n"
             (->> (for [y (range ymin (inc ymax))
                        x (range xmin (inc xmax))]
                    [x y])
                  (partition (inc (- xmax xmin)))
                  (map (fn [y-row]
                         (str (->> y-row
                                   (map (fn [position]
                                          (if (star? positions position) "#" ".")))
                                   (apply str))
                              "\n"))))))))


(deftest example
         (is= (-> (create-state test-data)
                  (time-tick)
                  (time-tick)
                  (time-tick)
                  (state->string))
              (str "After 3 seconds:\n"
                   "#...#..###\n"
                   "#...#...#.\n"
                   "#...#...#.\n"
                   "#####...#.\n"
                   "#...#...#.\n"
                   "#...#...#.\n"
                   "#...#...#.\n"
                   "#...#..###\n"))
         (is= (-> (create-state test-data)
                  (time-ticks 3)
                  (state->string))
              (str "After 3 seconds:\n"
                   "#...#..###\n"
                   "#...#...#.\n"
                   "#...#...#.\n"
                   "#####...#.\n"
                   "#...#...#.\n"
                   "#...#...#.\n"
                   "#...#...#.\n"
                   "#...#..###\n")))

(defn puzzle-solutions []
  (time (-> (get-puzzle-input)
            (create-state)
            (tick-until-smallest-shape)
            (state->string))))
