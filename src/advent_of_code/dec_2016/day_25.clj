(ns advent-of-code.dec-2016.day-25
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))


(def program (->> (slurp "src/advent_of_code/dec_2016/day_25_input.txt")
                  (string/split-lines)
                  (map (fn [command] (read-string (str "[" command "]"))))
                  (into [])))

(def state {:index          0
            :program        program
            :registers      (quote {a nil b 0 c 0 d 0})
            :last-2-outputs (list 1 0)})

(defn get-value
  {:test (fn []
           (is= (get-value {} -16) -16)
           (is= (get-value {} 16) 16))}
  [state x]
  (if (symbol? x) (get-in state [:registers x]) x))

(defmulti run (fn [_ command] (first command)))

(defmethod run 'cpy
  [state command]
  (let [[_ arg1 variable] command
        value (if (symbol? arg1) (get-in state [:registers arg1]) arg1)]
    (if-not (symbol? variable)
      state
      (assoc-in state [:registers variable] value))))

(defmethod run 'inc
  [state command]
  (let [variable (second command)]
    (update-in state [:registers variable] inc)))

(defmethod run 'dec
  [state command]
  (let [variable (second command)]
    (update-in state [:registers variable] dec)))

(defmethod run 'jnz
  [state command]
  (let [[_ arg1 arg2] command
        control (get-value state arg1)
        steps (get-value state arg2)]
    (if (zero? control)
      state
      (update state :index + (dec steps)))))

(defmethod run 'out
  [state command]
  (let [output (get-value state (second command))]
    (update state :last-2-outputs (fn [o] (take 2 (conj o output))))))

(def program-length (count program))

(def correct-last-2-outputs #{[0 1] [1 0]})

(defn run-program
  [state deja-vu]
  (let [command (get-in state [:program (:index state)])
        state (-> state
                  (run command)
                  (update :index inc))
        output (:last-2-outputs state)]

    (cond (or (>= (:index state) program-length)
              (not (contains? correct-last-2-outputs output)))
          :fail

          (contains? deja-vu state)
          state

          :else
          (recur state (conj deja-vu state)))))

(comment
  (run-program (assoc-in state [:registers 'a] 3)
               #{})

  (->> (range 2730)
       (some (fn [value]
               (let [state (run-program (assoc-in state [:registers 'a] value)
                                        #{})]
                 (when (not= state :fail)
                   [value state])))))

  ; 2730 too high

  (-> state
      (assoc-in [:registers 'a] 1)
      (run-program #{}))

  )