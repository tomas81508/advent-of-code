(ns advent-of-code.dec-2017.day-20
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :refer [split-lines]]))

(def input (slurp "src/advent_of_code/dec_2017/day_20_input.txt"))

(def test-input "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>\np=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>\np=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>\np=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>")

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input)
                [{:p [-6 0 0], :v [3 0 0], :a [0 0 0]}
                 {:p [-4 0 0], :v [2 0 0], :a [0 0 0]}
                 {:p [-2 0 0], :v [1 0 0], :a [0 0 0]}
                 {:p [3 0 0], :v [-1 0 0], :a [0 0 0]}]))}
  [input]
  (->> input
       (split-lines)
       (map (fn [row]
              (->> (re-seq #"-?\d+" row)
                   (partition 3)
                   (map (fn [x] (map read-string x))))))
       (map (fn [particle] {:p (first particle) :v (second particle) :a (nth particle 2)}))
       (vec)))

(def state (parse-input input))
(def test-state (parse-input test-input))

(comment
  ; slowest acceleration wins
  (->> state
       (map :a)
       (map-indexed (fn [index [x y z]] [index (+ (abs x) (abs y) (abs z))]))
       (reduce (fn [[best-index best-value] [index v]]
                 (if (< v best-value)
                   [index v]
                   [best-index best-value])))
       (first))
  ; => 150
  )

; part 2

(defn distance
  {:test (fn []
           (is= (distance [1 2 0] [0 0 2]) 9))}
  [p1 p2]
  (->> (map - p1 p2)
       (map (fn [x] (* x x)))
       (reduce +)))

(defn move
  {:test (fn []
           (is= (move {:p [-6 0 0] :v [3 0 0] :a [1 0 0]})
                {:a [1 0 0] :v [4 0 0] :p [-2 0 0]}))}
  [{p :p v :v a :a}]
  (let [v (map + v a)
        p (map + p v)]
    {:p p :v v :a a}))

(defn collide?
  {:test (fn []
           (is (collide? {:p [-6 0 0] :v [3 0 0] :a [0 0 0]}
                         {:p [-4 0 0] :v [2 0 0] :a [0 0 0]}))
           (is-not (collide? {:p [2920 873 306], :v [419 124 43], :a [31 9 3]} {:p [678 2930 1292], :v [98 415 179] :a [2 29 12]}))
           (is-not (collide? {:p [-6 0 0] :v [3 0 0] :a [0 0 0]}
                             {:p [-4 0 0] :v [-2 0 0] :a [0 0 0]}))
           (is-not (collide? {:p [1 0 0] :v [1 0 0] :a [0 0 0]}
                             {:p [2 0 0] :v [1 0 0] :a [0 0 0]}))
           (is-not (collide? {:p [-10 0 0] :v [2 0 0] :a [0 0 0]}
                             {:p [9 0 0] :v [-2 0 0] :a [0 0 0]})))}
  [particle1 particle2]
  (loop [d (distance (:p particle1) (:p particle2))
         particle1 particle1
         particle2 particle2]
    (let [particle1 (move particle1)
          particle2 (move particle2)
          new-d (distance (:p particle1) (:p particle2))]
      (cond (zero? new-d)
            true

            (>= new-d d)
            false

            :else
            (recur new-d particle1 particle2)))))

(comment
  (-> (let [state state]
        (loop [index 0
               non-colliding-particles #{}]
          (let [particle-at-index (get state index)]
            (if-not particle-at-index
              non-colliding-particles
              (let [colliding-particle (loop [[p & ps] state]
                                         (when p
                                           (if (and (not= particle-at-index p)
                                                    (collide? particle-at-index p))
                                             p
                                             (recur ps))))]
                (if colliding-particle
                  (recur (inc index) non-colliding-particles)
                  (recur (inc index) (conj non-colliding-particles particle-at-index))))))))
      (count))
  )




