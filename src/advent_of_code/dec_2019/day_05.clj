(ns advent-of-code.dec-2019.day-05
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_05.txt") $
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
  [program modes parameter i]
  (if (and (zero? (get modes parameter))
           (not= parameter :3))
    (nth program i)
    i))

(defn update-program
  [program position value]
  (assoc program position value))

(defmulti run-instruction
          (fn [program index _]
            (mod (nth program index) 100)))

(defmethod run-instruction 1
  [program index args]
  (let [[i0 i1 i2 i3] (subvec program index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value program modes :1 i1)
        v2 (get-value program modes :2 i2)]
    [(update-program program i3 (+ v1 v2))
     (+ index 4)
     args]))

(defmethod run-instruction 2
  [program index args]
  (let [[i0 i1 i2 i3] (subvec program index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value program modes :1 i1)
        v2 (get-value program modes :2 i2)]
    [(update-program program i3 (* v1 v2))
     (+ index 4)
     args]))

(defmethod run-instruction 3
  [program index args]
  (let [[_ i1] (subvec program index (+ 2 index))]
    [(update-program program i1 (:input args))
     (+ index 2)
     {}]))

(defmethod run-instruction 4
  [program index args]
  (let [[i0 i1] (subvec program index (+ 2 index))
        modes (get-modes i0)]
    [program
     (+ index 2)
     (assoc args :output (get-value program modes :1 i1))]))

; jump-if-true
(defmethod run-instruction 5
  [program index args]
  (let [[i0 i1 i2] (subvec program index (+ 3 index))
        modes (get-modes i0)
        v1 (get-value program modes :1 i1)
        v2 (get-value program modes :2 i2)]
    [program
     (if (zero? v1) (+ index 3) v2)
     args]))

; jump-if-false
(defmethod run-instruction 6
  [program index args]
  (let [[i0 i1 i2] (subvec program index (+ 3 index))
        modes (get-modes i0)
        v1 (get-value program modes :1 i1)
        v2 (get-value program modes :2 i2)]
    [program
     (if (zero? v1) v2 (+ index 3))
     args]))

; less than
(defmethod run-instruction 7
  [program index args]
  (let [[i0 i1 i2 i3] (subvec program index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value program modes :1 i1)
        v2 (get-value program modes :2 i2)]
    [(update-program program i3 (if (< v1 v2) 1 0))
     (+ index 4)
     args]))

; equals
(defmethod run-instruction 8
  [program index args]
  (let [[i0 i1 i2 i3] (subvec program index (+ 4 index))
        modes (get-modes i0)
        v1 (get-value program modes :1 i1)
        v2 (get-value program modes :2 i2)]
    [(update-program program i3 (if (= v1 v2) 1 0))
     (+ index 4)
     args]))

(defmethod run-instruction 99
  [_ index args]
  [:exit index args])

(defn run
  {:test (fn []
           (is= (run [3 9 8 9 10 9 4 9 99 -1 8] 7) 0)
           (is= (run [3 9 8 9 10 9 4 9 99 -1 8] 8) 1)
           (is= (run [3 9 7 9 10 9 4 9 99 -1 8] 5) 1)
           (is= (run [3 9 7 9 10 9 4 9 99 -1 8] 9) 0)
           (is= (run [3 3 1108 -1 8 3 4 3 99] 7) 0)
           (is= (run [3 3 1108 -1 8 3 4 3 99] 8) 1)
           (is= (run [3 3 1107 -1 8 3 4 3 99] 5) 1)
           (is= (run [3 3 1107 -1 8 3 4 3 99] 9) 0)
           (is= (run [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] 0) 0)
           (is= (run [3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9] 5) 1)
           (is= (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 0) 0)
           (is= (run [3 3 1105 -1 9 1101 0 0 12 4 12 99 1] 6) 1))}
  [program input]
  (loop [program program
         index 0
         args {:input input :output nil}]
    (let [[updated-program index args] (run-instruction program index args)]
      (if (= updated-program :exit)
        (:output args)
        (recur updated-program
               index
               args)))))


(deftest larger-example
         (let [program [3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                        1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                        999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99]]
           (is= (run program 5) 999)
           (is= (run program 8) 1000)
           (is= (run program 51) 1001)))


(deftest puzzle-a
         (is= (time (-> (get-puzzle-input)
                        (run 1)))
              ; "Elapsed time: 7.136423 msecs"
              16348437))

(deftest puzzle-b
         (is= (time (-> (get-puzzle-input)
                        (run 5)))
              ; "Elapsed time: 7.033396 msecs"
              6959377))


