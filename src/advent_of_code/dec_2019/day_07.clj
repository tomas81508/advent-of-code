(ns advent-of-code.dec-2019.day-07
  (:require [ysera.test :refer [is is-not is= deftest]]
            [clojure.math.combinatorics :refer [permutations]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_07.txt") $
        (clojure.string/split $ #",")
        (map read-string $)
        (vec $)))

(defn get-modes
  {:test (fn []
           (is= (get-modes 3)
                {:1 0
                 :2 0
                 :3 0})
           (is= (get-modes 1002)
                {:1 0
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

(defn get-value
  [int-code modes parameter i]
  (if (and (zero? (get modes parameter))
           (not= parameter :3))
    (nth int-code i)
    i))

(defn update-int-code
  [int-code position value]
  (assoc int-code position value))

(defmulti run-instruction
          (fn [{int-code :int-code
                index    :index}]
            (mod (nth int-code index)
                 100)))

(defmethod run-instruction 1
  [{int-code :int-code
    index    :index
    :as      program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes :1 i1)
        v2 (get-value int-code modes :2 i2)]
    (-> program
        (update :int-code update-int-code i3 (+ v1 v2))
        (update :index + 4))))

(defmethod run-instruction 2
  [{int-code :int-code
    index    :index
    :as      program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes :1 i1)
        v2 (get-value int-code modes :2 i2)]
    (-> program
        (update :int-code update-int-code i3 (* v1 v2))
        (update :index + 4))))

(defmethod run-instruction 3
  [{int-code :int-code
    index    :index
    inputs   :inputs
    :as      program}]
  (let [[_ i1] (subvec int-code index (+ 2 index))]
    (if-let [input (first inputs)]
      (-> program
          (update :int-code update-int-code i1 input)
          (update :index + 2)
          (update :inputs (partial drop 1))
          (dissoc program :awaiting-input))
      (assoc program :awaiting-input true))))

(defmethod run-instruction 4
  [{int-code :int-code
    index    :index
    :as      program}]
  (let [[i0 i1] (subvec int-code index (+ 2 index))
        modes (get-modes i0)]
    (-> program
        (update :index + 2)
        (update :outputs conj (get-value int-code modes :1 i1)))))

; jump-if-true
(defmethod run-instruction 5
  [{int-code :int-code
    index    :index
    :as      program}]
  (let [[i0 i1 i2] (subvec int-code index (+ 3 index))
        modes (get-modes i0)
        v1 (get-value int-code modes :1 i1)
        v2 (get-value int-code modes :2 i2)]
    (-> program
        (update :index (fn [index] (if (zero? v1) (+ index 3) v2))))))

; jump-if-false
(defmethod run-instruction 6
  [{int-code :int-code
    index    :index
    :as      program}]
  (let [[i0 i1 i2] (subvec int-code index (+ 3 index))
        modes (get-modes i0)
        v1 (get-value int-code modes :1 i1)
        v2 (get-value int-code modes :2 i2)]
    (-> program
        (update :index (fn [index] (if (zero? v1) v2 (+ index 3)))))))

; less than
(defmethod run-instruction 7
  [{int-code :int-code
    index    :index
    :as      program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes :1 i1)
        v2 (get-value int-code modes :2 i2)]
    (-> program
        (update :int-code update-int-code i3 (if (< v1 v2) 1 0))
        (update :index + 4))))

; equals
(defmethod run-instruction 8
  [{int-code :int-code
    index    :index
    :as      program}]
  (let [[i0 i1 i2 i3] (subvec int-code index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value int-code modes :1 i1)
        v2 (get-value int-code modes :2 i2)]
    (-> program
        (update :int-code update-int-code i3 (if (= v1 v2) 1 0))
        (update :index + 4))))

(defmethod run-instruction 99
  [program]
  (assoc program :halted true))

(defn run
  {:test (fn []
           (is= (run [3 9 8 9 10 9 4 9 99 -1 8] [7])
                {:int-code [3 9 8 9 10 9 4 9 99 0 8] :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 9 8 9 10 9 4 9 99 -1 8] [8])
                {:int-code [3 9 8 9 10 9 4 9 99 1 8] :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 9 7 9 10 9 4 9 99 -1 8] [5])
                {:int-code [3 9 7 9 10 9 4 9 99 1 8] :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 9 7 9 10 9 4 9 99 -1 8] [9])
                {:int-code [3 9 7 9 10 9 4 9 99 0 8] :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 3 1108 -1 8 3 4 3 99] [7])
                {:int-code [3 3 1108 0 8 3 4 3 99] :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 3 1108 -1 8 3 4 3 99] [8])
                {:int-code [3 3 1108 1 8 3 4 3 99] :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 3 1107 -1 8 3 4 3 99] [5])
                {:int-code [3 3 1107 1 8 3 4 3 99] :index 8 :inputs [] :outputs [1] :halted true})
           (is= (run [3 3 1107 -1 8 3 4 3 99] [9])
                {:int-code [3 3 1107 0 8 3 4 3 99] :index 8 :inputs [] :outputs [0] :halted true})
           (is= (run [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [0])
                {:int-code [3 12 6 12 15 1 13 14 13 4 13 99 0 0 1 9] :index 11 :inputs [] :outputs [0] :halted true})
           (is= (run [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] [5])
                {:int-code [3 12 6 12 15 1 13 14 13 4 13 99 5 1 1 9] :index 11 :inputs [] :outputs [1] :halted true})
           (is= (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [0])
                {:int-code [3 3 1105 0 9 1101 0 0 12 4 12 99 0] :index 11 :inputs [] :outputs [0] :halted true})
           (is= (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] [6])
                {:int-code [3 3 1105 6 9 1101 0 0 12 4 12 99 1] :index 11 :inputs [] :outputs [1] :halted true}))}
  ([program] (run program nil))
  ([program inputs]
   (loop [program (if (map? program)
                    program
                    {:int-code program
                     :index    0
                     :inputs   inputs
                     :outputs  []})]
     (let [program (run-instruction program)]
       (if (or (contains? program :halted)
               (contains? program :awaiting-input))
         program
         (recur program))))))


(deftest larger-example
         (let [program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                        1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                        999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
           (is= (:outputs (run program [5])) [999])
           (is= (:outputs (run program [8])) [1000])
           (is= (:outputs (run program [51])) [1001])))


(defn run-amplifiers
  {:test (fn []
           (is= (-> [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0]
                    (run-amplifiers [4 3 2 1 0]))
                43210)
           (is= (-> [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0]
                    (run-amplifiers [0 1 2 3 4]))
                54321)
           (is= (-> [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0]
                    (run-amplifiers [1 0 4 3 2]))
                65210))}
  [program phase-settings]
  (loop [input 0
         [phase & phases] phase-settings]
    (if-not phase
      input
      (let [{outputs :outputs} (run program [phase input])]
        (recur (last outputs) phases)))))

(defn get-max-thrusters
  {:test (fn []
           (is= (get-max-thrusters [3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0])
                43210)
           (is= (get-max-thrusters [3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0])
                54321)
           (is= (get-max-thrusters [3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0])
                65210))}
  [program]
  (->> (range 5)
       (permutations)
       (pmap (partial run-amplifiers program))
       (apply max)))

(deftest puzzle-a
         (is= (time (-> (get-puzzle-input)
                        (get-max-thrusters)))
              ; "Elapsed time: 16.31669 msecs"
              13848))

(defn run-amplifiers-loop
  {:test (fn []
           (is= (run-amplifiers-loop [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26 27 4 27 1001 28 -1 28 1005 28 6 99 0 0 5]
                                     [9 8 7 6 5])
                139629729)
           (is= (run-amplifiers-loop [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10]
                                     [9 7 8 5 6])
                18216))}
  [program phases]
  (loop [system (-> (reduce (fn [a v]
                              (update a :amplifiers conj
                                      {:int-code program
                                       :inputs   [v]
                                       :outputs  []
                                       :index    0}))
                            {:amplifiers []
                             :signal     0
                             :current-index    0}
                            phases))
         i 0]
    (let [current-index (:current-index system)
          current-amplifier (get-in system [:amplifiers current-index])]
      (if (:halted current-amplifier)
        (:signal system)
        (let [system (-> system
                         (update-in [:amplifiers current-index :inputs] conj (:signal system))
                         (update-in [:amplifiers current-index] run))]
          (recur (-> system
                     (assoc :signal (last (get-in system [:amplifiers current-index :outputs])))
                     (update :current-index (fn [current-index] (mod (inc current-index) 5))))
                 (inc i)))))))

(defn get-loop-max-thrusters
  [program]
  (->> (range 5 10)
       (permutations)
       (pmap (partial run-amplifiers-loop program))
       (apply max)))

(deftest puzzle-b
         (is= (time (get-loop-max-thrusters (get-puzzle-input)))
              ; "Elapsed time: 66.647157 msecs"
              12932154))

