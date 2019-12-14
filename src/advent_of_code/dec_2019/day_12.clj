(ns advent-of-code.dec-2019.day-12
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn create-initial-moon-state
  {:test (fn []
           (is= (create-initial-moon-state "<x=17, y=-7, z=-11>")
                [17 -7 -11]))}
  [s]
  (->> (re-find #"<x=([^,]+), y=([^,]+), z=([^>]+)>" s)
       (drop 1)
       (map read-string)))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_12.txt") $
        (clojure.string/split-lines $)
        (map create-initial-moon-state $)))

(defn create-state
  [moon-states]
  (map (fn [ms] {:position (into [] ms)
                 :velocity [0 0 0]})
       moon-states))

(defn create-mutable-state
  [moon-states]
  (into-array (map (fn [ms]
                     (into-array [(into-array ms) (into-array [0 0 0])]))
                   moon-states)))

(defn update-velocities
  {:test (fn []
           (is= (update-velocities [{:position [-1 0 2] :velocity [0 0 0]}
                                    {:position [2 -10 -7] :velocity [0 0 0]}
                                    {:position [4 -8 8] :velocity [0 0 0]}
                                    {:position [3 5 -1] :velocity [0 0 0]}])
                [{:position [-1 0 2] :velocity [3 -1 -1]}
                 {:position [2 -10 -7] :velocity [1 3 3]}
                 {:position [4 -8 8] :velocity [-3 1 -3]}
                 {:position [3 5 -1] :velocity [-1 -3 1]}]))}
  [planets]
  (map (fn [{position :position :as moon}]
         (update moon :velocity
                 (fn [v]
                   (map + v
                        (reduce (fn [a p]
                                  (map + a
                                       (map (fn [index]
                                              (let [v (- (nth p index)
                                                         (nth position index))]
                                                (cond (pos? v) 1
                                                      (zero? v) 0
                                                      :else -1)))
                                            (range 3))))
                                [0 0 0]
                                (->> planets
                                     (remove (fn [m] (= m moon)))
                                     (map :position)))))))
       planets))

(defn update-positions
  {:test (fn []
           (is= (update-positions [{:position [-1 0 2] :velocity [3 -1 -1]}
                                   {:position [2 -10 -7] :velocity [1 3 3]}
                                   {:position [4 -8 8] :velocity [-3 1 -3]}
                                   {:position [3 5 -1] :velocity [-1 -3 1]}])
                [{:position [2 -1 1] :velocity [3 -1 -1]}
                 {:position [3 -7 -4] :velocity [1 3 3]}
                 {:position [1 -7 5] :velocity [-3 1 -3]}
                 {:position [2 2 0] :velocity [-1 -3 1]}]))}
  [planets]
  (->> planets
       (map (fn [planet]
              (update planet :position (fn [p] (map + p (:velocity planet))))))))

(defn step
  {:test (fn []
           (is= (nth (iterate step [{:position [-1 0 2] :velocity [0 0 0]}
                                    {:position [2 -10 -7] :velocity [0 0 0]}
                                    {:position [4 -8 8] :velocity [0 0 0]}
                                    {:position [3 5 -1] :velocity [0 0 0]}])
                     2)
                [{:position [5 -3 -1] :velocity [3 -2 -2]}
                 {:position [1 -2 2] :velocity [-2 5 6]}
                 {:position [1 -4 -1] :velocity [0 3 -6]}
                 {:position [1 -4 2] :velocity [-1 -6 2]}])
           (is= (nth (iterate step [{:position [-1 0 2] :velocity [0 0 0]}
                                    {:position [2 -10 -7] :velocity [0 0 0]}
                                    {:position [4 -8 8] :velocity [0 0 0]}
                                    {:position [3 5 -1] :velocity [0 0 0]}])
                     10)
                [{:position [2 1 -3] :velocity [-3 -2 1]}
                 {:position [1 -8 0] :velocity [-1 1 3]}
                 {:position [3 -6 1] :velocity [3 2 -3]}
                 {:position [2 0 4] :velocity [1 -1 -1]}])
           )}
  [planets]
  (->> planets
       (update-velocities)
       (update-positions)))

(defn abs [x] (if (pos? x) x (- x)))

(defn calculate-energy
  {:test (fn []
           (is= (-> (iterate step [{:position [-1 0 2] :velocity [0 0 0]}
                                   {:position [2 -10 -7] :velocity [0 0 0]}
                                   {:position [4 -8 8] :velocity [0 0 0]}
                                   {:position [3 5 -1] :velocity [0 0 0]}])
                    (nth 10)
                    (calculate-energy))
                179))}
  [planets]
  (reduce (fn [a planet]
            (+ a
               (* (->> (:position planet)
                       (map abs)
                       (apply +))
                  (->> (:velocity planet)
                       (map abs)
                       (apply +)))))
          0
          planets))

(deftest puzzle-a
         (is= (time (as-> (get-puzzle-input) $
                          (create-state $)
                          (iterate step $)
                          (nth $ 1000)
                          (calculate-energy $)))
              ; "Elapsed time: 57.539504 msecs"
              9441))

(defn detect-cycle-of-axis
  {:test (fn []
           (is= (detect-cycle-of-axis [-1 2 4 3] [0 0 0 0])
                18))}
  [ps vs]
  (let [initial-positions ps
        initial-velocity vs]
    (loop [counter 0
           ps ps
           vs vs]
      (if (and (not (zero? counter))
               (= initial-positions ps)
               (= initial-velocity vs))
        counter
        (let [new-vs (for [i (range 4)]
                       (let [p (nth ps i)
                             other-ps (->> (for [j (range 4) :when (not= j i)] j)
                                           (map (fn [j] (nth ps j))))
                             v-change (->> other-ps
                                           (map (fn [other-p]
                                                  (let [v (- other-p p)]
                                                    (cond (pos? v) 1
                                                          (zero? v) 0
                                                          :else -1))))
                                           (apply +))]
                         (+ (nth vs i) v-change)))
              new-ps (for [i (range 4)]
                       (+ (nth ps i) (nth new-vs i)))]
          (recur (inc counter)
                 new-ps
                 new-vs))))))

(defn gcd
  {:test (fn []
           (is= (gcd 71 83) 1)
           (is= (gcd 71 0) 71)
           (is= (gcd 1244 432) 4))}
  [a b]
  {:pre [a b]}
  (if (zero? b) (if (pos? a) a (- a)) (recur b (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))

(defn detect-cycle
  {:test (fn []
           (is= (time (detect-cycle [{:position [-1 0 2] :velocity [0 0 0]}
                                     {:position [2 -10 -7] :velocity [0 0 0]}
                                     {:position [4 -8 8] :velocity [0 0 0]}
                                     {:position [3 5 -1] :velocity [0 0 0]}]))
                2772))}
  [planets]
  (let [xs (map (fn [planet] (get-in planet [:position 0])) planets)
        vxs (map (fn [planet] (get-in planet [:velocity 0])) planets)
        ys (map (fn [planet] (get-in planet [:position 1])) planets)
        vys (map (fn [planet] (get-in planet [:velocity 1])) planets)
        zs (map (fn [planet] (get-in planet [:position 2])) planets)
        vzs (map (fn [planet] (get-in planet [:velocity 2])) planets)
        cycle-x (detect-cycle-of-axis xs vxs)
        cycle-y (detect-cycle-of-axis ys vys)
        cycle-z (detect-cycle-of-axis zs vzs)]
    (lcmv cycle-x cycle-y cycle-z)))

(deftest puzzle-b
         (is= (time (as-> (get-puzzle-input) $
                          (create-state $)
                          (into [] $)
                          (detect-cycle $)))
              503560201099704))

