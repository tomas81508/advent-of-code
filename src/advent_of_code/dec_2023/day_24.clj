(ns advent-of-code.dec-2023.day-24
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.test :refer [deftest]]))

(defn parse-input
  {:test (fn []
           (is= (parse-input "19, 13, 30 @ -2,  1, -2")
                {:point [19 13 30] :velocity [-2 1 -2]}))}
  [line]
  (let [[px py pz vx vy vz] (->> (re-seq #"[\d-]+" line)
                                 (map read-string))]
    {:point [px py pz] :velocity [vx vy vz]}))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_24_input.txt")
                (clojure.string/split-lines)
                (map parse-input)
                (into [])))

(def test-input (->> ["19, 13, 30 @ -2,  1, -2"
                      "18, 19, 22 @ -1, -1, -2"
                      "20, 25, 34 @ -2, -2, -4"
                      "12, 31, 28 @ -1, -2, -1"
                      "20, 19, 15 @  1, -5, -3"]
                     (map parse-input)
                     (into [])))

(defn parameter-form->normal-form
  [{[p1 p2 _] :point [v1 v2 _] :velocity}]
  [v2 (- v1) (-' (*' v1 p2) (*' v2 p1))])

(defn parallell?
  {:test (fn []
           (is (parallell? {:velocity [-1 -1 -2]} {:velocity [2 2 4]}))
           (is-not (parallell? {:velocity [-1 -2 -2]} {:velocity [1 1 4]})))}
  [{v1 :velocity} {v2 :velocity}]
  (->> (map (fn [c1 c2] [c1 c2]) (drop-last v1) (drop-last v2))
       (reduce (fn [q [c1 c2]]
                 (cond (and (zero? c1) (zero? c2)) q
                       (or (zero? c1) (zero? c2)) (reduced false)
                       (nil? q) (/ c1 c2)
                       (not= q (/ c1 c2)) (reduced false)
                       :else q))
               nil)))

(defn find-intersection-xy
  "a1x+b1y+c1=0 and a2x+b2y+c2=0
   x= (b1c2-b2c1)/(a1b2-a2b1)
   y=(c1a2-c2a1)/(a1b2-a2b1)"
  {:test (fn []
           (is= (find-intersection-xy {:point [19 13 30], :velocity [-2 1 -2]}
                                      {:point [18 19 22], :velocity [-1 -1 -2]})
                [43/3 46/3])
           (is-not (find-intersection-xy {:point [18 19 22], :velocity [-1 -1 -2]}
                                         {:point [20 25 34], :velocity [-2 -2 -4]}))
           )}
  [l1 l2]
  (when-not (parallell? l1 l2)
    (let [[a1 b1 c1] (parameter-form->normal-form l1)
          [a2 b2 c2] (parameter-form->normal-form l2)
          _ (when (zero? (-' (*' a1 b2) (*' a2 b1)))
              (println l1 l2))
          x (/ (-' (*' b1 c2) (*' b2 c1))
               (-' (*' a1 b2) (*' a2 b1)))
          y (/ (-' (*' a2 c1) (*' a1 c2))
               (-' (*' a1 b2) (*' a2 b1)))]
      [x y])))

(defn happened-in-the-past?
  {:test (fn []
           (is (happened-in-the-past? {:point [19 13 30] :velocity [-2 1 -2]} [193/9 106/9]))
           (is-not (happened-in-the-past? {:point [20 19 15] :velocity [1 -5 -3]} [193/9 106/9])))}
  [{[p1 _ _] :point [v1 _ _] :velocity} [x _]]
  (or (and (< p1 x) (neg? v1))
      (and (< x p1) (pos? v1))))

(defn find-intersections-inside-box
  {:test (fn []
           (is= (find-intersections-inside-box [{:point [19 13 30], :velocity [-2 1 -2]}
                                                {:point [18 19 22], :velocity [-1 -1 -2]}]
                                               [7 27])
                1)
           ; outside of test area
           (is= (find-intersections-inside-box [{:point [19 13 30], :velocity [-2 1 -2]}
                                                {:point [12 31 28], :velocity [-1 -2 -1]}]
                                               [7 27])
                0)
           (is= (find-intersections-inside-box test-input [7 27])
                2))}
  [lines [tb1 tb2]]
  (->> (combinations lines 2)
       (keep (fn [[l1 l2]]
               (when-let [intersection (find-intersection-xy l1 l2)]
                 (when (and (<= tb1 (first intersection) tb2)
                            (<= tb1 (second intersection) tb2))
                   [l1 l2 intersection]))))
       (remove (fn [[l1 l2 intersection]]
                 (or (happened-in-the-past? l1 intersection)
                     (happened-in-the-past? l2 intersection))))
       (count)))

(deftest puzzle-a
  (is= (time (find-intersections-inside-box input [200000000000000 400000000000000]))
       25261))













