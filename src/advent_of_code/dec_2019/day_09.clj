(ns advent-of-code.dec-2019.day-09
  (:require [ysera.test :refer [is is-not is= deftest]]
            [clojure.math.combinatorics :refer [permutations]]))

(def BOOST
  (as-> (slurp "src/advent_of_code/dec_2019/day_09.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)))

(defn get-modes
  {:test (fn []
           (is= (get-modes 3)
                {:1 0
                 :2 0
                 :3 0})
           (is= (get-modes 1202)
                {:1 2
                 :2 1
                 :3 0})
           (is= (get-modes 11003)
                {:1 0
                 :2 1
                 :3 1}))}
  [number]
  (let [number-string (clojure.string/join "" (take-last 5 (str "0000" number)))]
    {:1 (read-string (subs number-string 2 3))
     :2 (read-string (subs number-string 1 2))
     :3 (read-string (subs number-string 0 1))}))

(defn get-write-position
  [modes relative-base parameter i]
  (case (get modes parameter)
    0 i
    2 (+ i relative-base)))

(defn get-value
  [int-code modes relative-base parameter i]
  (case (get modes parameter)
    0 (get int-code i 0)
    1 i
    2 (get int-code (+ i relative-base) 0)))

(defn update-int-code
  [int-code position value]
  (-> (if (<= (count int-code) position)
        (->> (concat int-code (repeat (inc (- position (count int-code))) 0))
             (vec))
        int-code)
      (assoc position value)))

(defmulti run-instruction
          (fn [{int-code :int-code
                index    :index}]
            (mod (nth int-code index)
                 100)))

(defmethod run-instruction 1
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes relative-base :1 i1)
        v2 (get-value int-code modes relative-base :2 i2)
        w3 (get-write-position modes relative-base :3 i3)]
    (-> program
        (update :int-code update-int-code w3 (+ v1 v2))
        (update :index + 4))))

(defmethod run-instruction 2
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes relative-base :1 i1)
        v2 (get-value int-code modes relative-base :2 i2)
        w3 (get-write-position modes relative-base :3 i3)]
    (-> program
        (update :int-code update-int-code w3 (* v1 v2))
        (update :index + 4))))

(defmethod run-instruction 3
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    inputs        :inputs
    :as           program}]
  (let [[i0 i1] (subvec int-code index (+ 2 index))
        modes (get-modes i0)
        w1 (get-write-position modes relative-base :1 i1)]
    (if-let [input (first inputs)]
      (-> program
          (update :int-code update-int-code w1 input)
          (update :index + 2)
          (update :inputs (partial drop 1))
          (dissoc program :awaiting-input))
      (assoc program :awaiting-input true))))

(defmethod run-instruction 4
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1] (subvec int-code index (+ 2 index))
        modes (get-modes i0)]
    (-> program
        (update :index + 2)
        (update :outputs conj (get-value int-code modes relative-base :1 i1)))))

; jump-if-true
(defmethod run-instruction 5
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1 i2] (subvec int-code index (+ 3 index))
        modes (get-modes i0)
        v1 (get-value int-code modes relative-base :1 i1)
        v2 (get-value int-code modes relative-base :2 i2)]
    (-> program
        (update :index (fn [index] (if (zero? v1) (+ index 3) v2))))))

; jump-if-false
(defmethod run-instruction 6
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1 i2] (subvec int-code index (+ 3 index))
        modes (get-modes i0)
        v1 (get-value int-code modes relative-base :1 i1)
        v2 (get-value int-code modes relative-base :2 i2)]
    (-> program
        (update :index (fn [index] (if (zero? v1) v2 (+ index 3)))))))

; less than
(defmethod run-instruction 7
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes relative-base :1 i1)
        v2 (get-value int-code modes relative-base :2 i2)
        w3 (get-write-position modes relative-base :3 i3)]
    (-> program
        (update :int-code update-int-code w3 (if (< v1 v2) 1 0))
        (update :index + 4))))

; equals
(defmethod run-instruction 8
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes relative-base :1 i1)
        v2 (get-value int-code modes relative-base :2 i2)
        w3 (get-write-position modes relative-base :3 i3)]
    (-> program
        (update :int-code update-int-code w3 (if (= v1 v2) 1 0))
        (update :index + 4))))

(defmethod run-instruction 9
  [{int-code      :int-code
    index         :index
    relative-base :relative-base
    :as           program}]
  (let [[i0 i1] (subvec int-code index (+ 2 index))
        modes (get-modes i0)]
    (-> program
        (update :index + 2)
        (update :relative-base + (get-value int-code modes relative-base :1 i1)))))

(defmethod run-instruction 99
  [program]
  (assoc program :halted true))

(defn create-program
  [int-code inputs]
  {:int-code      int-code
   :index         0
   :relative-base 0
   :inputs        inputs
   :outputs       []})

(defn run
  "Runs a program until it halts or require input."
  {:test (fn []
           (is= (run [3 9 8 9 10 9 4 9 99 -1 8] [7])
                {:int-code [3 9 8 9 10 9 4 9 99 0 8] :relative-base 0 :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 9 8 9 10 9 4 9 99 -1 8] [8])
                {:int-code [3 9 8 9 10 9 4 9 99 1 8] :relative-base 0 :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 9 7 9 10 9 4 9 99 -1 8] [5])
                {:int-code [3 9 7 9 10 9 4 9 99 1 8] :relative-base 0 :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 9 7 9 10 9 4 9 99 -1 8] [9])
                {:int-code [3 9 7 9 10 9 4 9 99 0 8] :relative-base 0 :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 3 1108 -1 8 3 4 3 99] [7])
                {:int-code [3 3 1108 0 8 3 4 3 99] :relative-base 0 :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 3 1108 -1 8 3 4 3 99] [8])
                {:int-code [3 3 1108 1 8 3 4 3 99] :relative-base 0 :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 3 1107 -1 8 3 4 3 99] [5])
                {:int-code [3 3 1107 1 8 3 4 3 99] :relative-base 0 :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 3 1107 -1 8 3 4 3 99] [9])
                {:int-code [3 3 1107 0 8 3 4 3 99] :relative-base 0 :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [0])
                {:int-code [3 12 6 12 15 1 13 14 13 4 13 99 0 0 1 9] :relative-base 0 :index 11 :inputs [] :outputs [0] :halted true})
           (is= (run [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [5])
                {:int-code [3 12 6 12 15 1 13 14 13 4 13 99 5 1 1 9] :relative-base 0 :index 11 :inputs [] :outputs [1] :halted true})
           (is= (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [0])
                {:int-code [3 3 1105 0 9 1101 0 0 12 4 12 99 0] :relative-base 0 :index 11 :inputs [] :outputs [0] :halted true})
           (is= (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [6])
                {:int-code [3 3 1105 6 9 1101 0 0 12 4 12 99 1] :relative-base 0 :index 11 :inputs [] :outputs [1] :halted true})
           (let [int-code [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]]
             (is= (:outputs (run int-code []))
                  int-code))
           (is= (:outputs (run [1102 34915192 34915192 7 4 7 99 0] []))
                [1219070632396864])
           (is= (:outputs (run [104 1125899906842624 99] []))
                [1125899906842624]))}
  ([program] (run program nil))
  ([program inputs]
   (loop [program (if (map? program)
                    (assoc program :inputs inputs)
                    (create-program program inputs))]
     (let [program (run-instruction program)]
       (if (or (contains? program :halted)
               (contains? program :awaiting-input))
         program
         (recur program))))))

(deftest puzzle-a
         (is= (first (:outputs (run BOOST [1])))
              2399197539))

(deftest puzzle-b
         (is= (first (:outputs (run BOOST [2])))
              35106))