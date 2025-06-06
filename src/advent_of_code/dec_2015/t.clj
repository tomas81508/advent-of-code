(ns advent-of-code.dec-2015.t
  (:require [advent-of-code.test :refer [is=]]))

(def input (slurp "src/advent_of_code/dec_2015/day_23_input.txt"))
(def test-input "inc a\njio a, +2\ntpl a\ninc a")

(defn half
  [state r]
  (-> state
      (update r / 2)
      (update :position inc)))

(defn triple
  [state r]
  (-> state
      (update r * 3)
      (update :position inc)))

(defn inc-r
  [state r]
  (-> state
      (update r inc)
      (update :position inc)))

(defn jump
  [state offset]
  (update state :position + offset))

(defn jump-if-even
  [state r offset]
  (if (even? (get state r))
    (update state :position + offset)
    (update state :position inc)))

(defn jump-if-one
  [state r offset]
  (if (= 1 (get state r))
    (update state :position + offset)
    (update state :position inc)))

(defn do-instruction
  [state instruction]
  (cond
    (re-find #"hlf (\w)" instruction)
    (let [[_ r] (re-find #"hlf (\w)" instruction)]
      (half state r))

    (re-find #"tpl (\w)" instruction)
    (let [[_ r] (re-find #"tpl (\w)" instruction)]
      (triple state r))

    (re-find #"inc (\w)" instruction)
    (let [[_ r] (re-find #"inc (\w)" instruction)]
      (inc-r state r))

    (re-find #"jmp ([-+]?\d+)" instruction)
    (let [[_ offset] (re-find #"jmp ([-+]?\d+)" instruction)]
      (jump state (read-string offset)))

    (re-find #"jie (\w), ([-+]?\d+)" instruction)
    (let [[_ r offset] (re-find #"jie (\w), ([-+]?\d+)" instruction)]
      (jump-if-even state r (read-string offset)))

    (re-find #"jio (\w), ([-+]?\d+)" instruction)
    (let [[_ r offset] (re-find #"jio (\w), ([-+]?\d+)" instruction)]
      (jump-if-one state r (read-string offset)))))

(defn run-program
  {:test (fn []
           (is= (-> (clojure.string/split-lines test-input)
                    (run-program 0 0)
                    (get "a"))
                2))}
  [instructions a-start b-start]
  (loop [state {"a" a-start "b" b-start :position 0}]
    (if-let [instruction (nth instructions (:position state) nil)]
      (do (println instruction state)
        (recur (do-instruction state instruction)))
      state)))

(defn part-1
  [input]
  (-> (clojure.string/split-lines input)
      (run-program 0 0)
      (get "b")))

(defn part-2
  [input]
  (-> (clojure.string/split-lines input)
      (run-program 1 0)
      (get "b")))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 12.161792 msecs"
  ;=> 255

  (time (part-2 input))
  ;; "Elapsed time: 7.601625 msecs"
  ;=> 334
  )
