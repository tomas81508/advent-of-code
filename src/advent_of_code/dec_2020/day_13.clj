(ns advent-of-code.dec-2020.day-13
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_13.txt")
       (clojure.string/split-lines)))

(def test-input "939\n7,13,x,x,59,x,31,19")

(defn create-state
  {:test (fn []
           (is= (->> test-input
                     (clojure.string/split-lines)
                     (create-state))
                {:earliest-time 939
                 :buses         [[7 0] [13 0] [19 0] [31 0] [59 0]]}))}
  [input]
  {:earliest-time (read-string (first input))
   :buses         (as-> (second input) $
                        (clojure.string/split $ (re-pattern ","))
                        (filter (fn [id] (not= id "x")) $)
                        (map read-string $)
                        (sort $)
                        (map (fn [id] [id 0]) $))})

(defn get-earliest-bus
  {:test (fn []
           (is= (->> test-input
                     (clojure.string/split-lines)
                     (create-state)
                     (get-earliest-bus))
                [59 944]))}
  [{earliest-time :earliest-time
    buses         :buses}]
  (loop [buses buses]
    (if-not (some (fn [[_ time]] (< time earliest-time)) buses)
      (->> (sort-by second buses)
           (first))
      ; Need to force lazy seq, otherwise stack overflow
      (recur (doall
               (map (fn [[id time :as args]]
                      (if (>= time earliest-time)
                        args
                        [id (+ time id)]))
                    buses))))))

(deftest puzzle-a
         (let [state (->> (get-puzzle-input)
                          (create-state))
               result (get-earliest-bus state)]
           (is= result [677 1002637])
           (is= (* (- (second result) (:earliest-time state))
                   (first result))
                3385)))


(defn create-state-2 [input]
  (as-> input $
        (second $)
        (clojure.string/split $ #",")
        (map-indexed (fn [i x] [i (when-not (= x "x") (read-string x))]) $)
        (filter (fn [[_ x]] x) $)))

(defn get-first-common-number
  {:test (fn []
           (is= (get-first-common-number (range -1 400 13)
                                         (range -4 400 59))
                350))}
  [l1 l2]
  (let [n1 (if (neg? (first l1)) (second l1) (first l1))
        d1 (- (second l1) (first l1))
        n2 (if (neg? (first l2)) (second l2) (first l2))
        d2 (- (second l2) (first l2))
        algorithm (fn [large large-d small small-d]
                   (let [x (- large small)]
                     (loop [m 1]
                       (let [r (+ x (* m large-d))]
                         (if (zero? (mod r small-d))
                           (+ (* large-d m) large)
                           (recur (inc m)))))))]
    (if (> n1 n2)
      (algorithm n1 d1 n2 d2)
      (algorithm n2 d2 n1 d1))))

(defn create-sequence
  [index step]
  (->> (range)
       (map (fn [x] (+ (- step index) (* step x))))))

(defn solver-b
  {:test (fn []
           (is= (->> (clojure.string/split-lines test-input)
                     (solver-b))
                1068781))}
  [input]
  (->> (create-state-2 input)
       (reduce (fn [[i1 n1] [i2 n2]]
                 (let [seq1 (create-sequence i1 n1)
                       seq2 (create-sequence i2 n2)
                       first-common-number (get-first-common-number seq1 seq2)
                       n3 (* n1 n2)]
                   [(- n3 first-common-number) (* n1 n2)])))
       (apply -)
       (-))
  )

(deftest puzzle-b
         (is= (time (solver-b (get-puzzle-input)))
              ; "Elapsed time: 0.54216 msecs"
              600689120448303))

































