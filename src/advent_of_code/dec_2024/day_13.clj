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
  (if (zero? b) a (recur b (mod a b))))

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

(defn solve-with-elimination
  {:test (fn []
           (is= (solve-with-elimination {:ax 3 :bx 4 :ay 5 :by 6 :rx 10 :ry 14})
                [-2 4])
           (is= (solve-with-elimination (create-equation-data test-input1))
                [80 40]))}
  [{ax :ax bx :bx ay :ay by :by rx :rx ry :ry}]
  (let [a (/ (- (* rx by) (* ry bx))
             (- (* ax by) (* ay bx)))
        b (/ (- rx (* a ax)) bx)]
    [a b]))

(defn linearly-dependant?
  [{ax :ax bx :bx ay :ay by :by}]
  (= (/ ax bx)
     (/ ay by)))

(def big-number 10000000000000)
(def equations-2 (->> input
                      (map create-equation-data)
                      (map (fn [equation] (-> equation
                                              (update :rx + big-number)
                                              (update :ry + big-number))))
                      (filter should-we-even-try?)))

(comment
  ;part 1
  (time (->> equations
             (map solve-with-elimination)
             (filter (fn [[a b]] (and (integer? a) (pos? a) (integer? b) (pos? b))))
             (map (fn [[a b]] (+ (* 3 a) b)))
             (reduce +)))

  ; part 2
  (time (->> equations-2
             (map solve-with-elimination)
             (filter (fn [[a b]] (and (integer? a) (pos? a) (integer? b) (pos? b))))
             (map (fn [[a b]] (+ (* 3 a) b)))
             (reduce +)))

  )


