(ns advent-of-code.dec-2021.day-03
  (:require [ysera.test :refer [is= deftest]]))

(def input-data (->> (slurp "src/advent_of_code/dec_2021/day_03_input.txt")
                     (clojure.string/split-lines)))

(def test-data ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])

(defn calculate-gamma-rate
  {:test (fn []
           (is= (calculate-gamma-rate test-data)
                [1 0 1 1 0]))}
  [data]
  (let [count-of-test-data (count data)]
    (->> data
         (reduce (fn [a d]
                   (->> d
                        (map (fn [item] (if (= item \1) 1 0)))
                        ; map adds vectors
                        (map + a)))
                 (repeat (count (first data)) 0))
         ; example-data: [7 5 8 7 5]
         (map (fn [x]
                (if (> (/ x count-of-test-data) 1/2)
                  1
                  0))))))

(defn calculate-epsilon-rate
  {:test (fn []
           (is= (calculate-epsilon-rate [1 0 1 1 0])
                [0 1 0 0 1]))}
  [gamma-rate]
  (->> gamma-rate
       (map (fn [x] (if (zero? x) 1 0)))))

(defn parse-vec
  {:test (fn []
           (is= (parse-vec [1 0 1 1 0]) 22)
           (is= (parse-vec [0 1 0 0 1]) 9))}
  [rate]
  (-> (clojure.string/join "" rate)
      (Integer/parseInt 2)))

(defn parse-str
  {:test (fn []
           (is= (parse-vec "10110") 22)
           (is= (parse-vec "01001") 9))}
  [rate]
  (Integer/parseInt rate 2))

(defn solver-a
  {:test (fn []
           (is= (solver-a test-data)
                198))}
  [data]
  (let [gamma-rate (calculate-gamma-rate data)
        epsilon-rate (calculate-epsilon-rate gamma-rate)]
    (* (parse-vec gamma-rate) (parse-vec epsilon-rate))))

(deftest puzzle-a
         (is= (time (solver-a input-data))
              ; "Elapsed time: 3.743779 msecs"
              4139586))


(defn filter-data
  [data strategy]
  (let [max-index (count (first data))]
    (loop [index 0
           data data]
      (let [value-frequencies (->> data
                                   (map (fn [d] (nth d index)))
                                   (frequencies))
            ; {\0 5, \1 7}
            value-to-keep (strategy value-frequencies)
            filtered-data (->> data
                               (filter (fn [d] (= (nth d index) value-to-keep))))]
        (cond (= 1 (count filtered-data))
              (first filtered-data)

              (= (inc index) max-index)
              "Error"

              :else
              (recur (inc index) filtered-data))))))


(defn oxygen-generator-rating
  {:test (fn []
           (is= (oxygen-generator-rating test-data)
                23))}
  [data]
  (-> (filter-data data (fn [value-frequencies]
                          (if (>= (get value-frequencies \1)
                                  (get value-frequencies \0))
                            \1
                            \0)))
      (parse-str)))

(defn CO2-scrubber-rating
  {:test (fn []
           (is= (CO2-scrubber-rating test-data)
                10))}
  [data]
  (-> (filter-data data (fn [value-frequencies]
                          (if (< (get value-frequencies \1)
                                 (get value-frequencies \0))
                            \1
                            \0)))
      (parse-str)))

(defn solver-b
  {:test (fn []
           (is= (solver-b test-data)
                230))}
  [data]
  (let [oxygen-generator-rate (oxygen-generator-rating data)
        CO2-scrubber-rate (CO2-scrubber-rating data)]
    (* oxygen-generator-rate CO2-scrubber-rate)))

(deftest puzzle-b
         (is= (time (solver-b input-data))
              ; "Elapsed time: 1.465716 msecs"
              1800151)
         )



