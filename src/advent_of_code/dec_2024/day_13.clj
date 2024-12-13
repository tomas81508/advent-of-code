(ns advent-of-code.dec-2024.day-13
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def test-input1 ["Button A: X+94, Y+34"
                  "Button B: X+22, Y+67"
                  "Prize: X=8400, Y=5400"])

(def test-input2 ["Button A: X+26, Y+66"
                  "Button B: X+67, Y+21"
                  "Prize: X=12748, Y=12176"])

(def input (->> (slurp "src/advent_of_code/dec_2024/day_13_input.txt")
                (clojure.string/split-lines)
                (remove (fn [row] (= row "")))
                (partition 3)))

(defn create-equation-data
  {:test (fn []
           (is= (create-equation-data test-input1)
                {:ax 94 :ay 34
                 :bx 22 :by 67
                 :rx 8400 :ry 5400}))}
  [[a-stuff b-stuff r-stuff]]
  (let [[ax ay] (map read-string (re-seq #"\d+" a-stuff))
        [bx by] (map read-string (re-seq #"\d+" b-stuff))
        [rx ry] (map read-string (re-seq #"\d+" r-stuff))]
    {:ax ax :ay ay :bx bx :by by :rx rx :ry ry}))

(def test-equation-1 (create-equation-data test-input1))
(def test-equation-2 (create-equation-data test-input2))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn should-we-even-try?
  {:test (fn []
           (is (should-we-even-try? test-equation-1))
           (is-not (should-we-even-try? test-equation-2)))}
  [{ax :ax bx :bx ay :ay by :by rx :rx ry :ry}]
  (and (let [cdx (gcd ax bx)]
         (zero? (rem rx cdx)))
       (let [cdy (gcd ay by)]
         (zero? (rem ry cdy)))))

(def equations (->> input
                    (map create-equation-data)
                    (filter should-we-even-try?)))

(defn check-equation
  {:test (fn []
           (is= (check-equation test-equation-1 80 40) :solution)
           (is= (check-equation test-equation-1 20 40) :too-low)
           (is= (check-equation test-equation-1 90 40) :too-high))}
  [{ax :ax bx :bx ay :ay by :by rx :rx ry :ry} a b]
  (let [actual-x (+ (* a ax) (* b bx))
        actual-y (+ (* a ay) (* b by))]
    (cond (or (> actual-x rx) (> actual-y ry))
          :too-high

          (or (< actual-x rx) (< actual-y ry))
          :too-low

          :else :solution)))

(defn find-solutions
  {:test (fn []
           (is= (find-solutions test-equation-1)
                [{:a 80 :b 40}]))}
  [equation]
  (loop [[b & rbs] (range 101)
         solutions []]
    (if-not b
      solutions
      (let [result (loop [[a & ras] (range 101)]
                     (if-not a
                       :too-low
                       (let [result (check-equation equation a b)]
                         (case result
                           :too-low (recur ras)
                           :solution a
                           :too-high :too-high))))]
        (if (number? result)
          (recur rbs (conj solutions {:a result :b b}))
          (recur rbs solutions))))))

(defn find-cheapest-solution-cost
  {:test (fn []
           (is= (find-cheapest-solution-cost [{:a 80 :b 40}
                                              {:a 60 :b 90}])
                270))}
  [solutions]
  (->> solutions
       (map (fn [s] (+ (* 3 (:a s)) (:b s))))
       (apply min)))

(comment
  (time (->> equations
             (map find-solutions)
             (remove empty?)
             (map find-cheapest-solution-cost)
             (reduce +)))
  ; 36571
  )

; part two



