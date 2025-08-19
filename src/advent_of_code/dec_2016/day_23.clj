(ns advent-of-code.dec-2016.day-23
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))


(def program (->> (slurp "src/advent_of_code/dec_2016/day_23_input.txt")
                  (string/split-lines)
                  (map (fn [command] (read-string (str "[" command "]"))))
                  (into [])))

(def state {:index     0
            :program   program
            :registers (quote {a 7 b 0 c 0 d 0})})

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

(defmethod run 'tgl
  [state command]
  (let [arg (second command)
        affected-index (+ (:index state) (get-value state arg))]
    (if-not (< affected-index (count (:program state)))
      state
      (update-in state [:program affected-index]
                 (fn [command]
                   (update command 0
                           (fn [operation]
                             (condp = operation
                               'inc 'dec
                               'dec 'inc
                               'tgl 'inc
                               'jnz 'cpy
                               'cpy 'jnz))))))))

(def test-program (quote [[cpy 2 a] [tgl a] [tgl a] [tgl a] [cpy 1 a] [dec a] [dec a]]))

(def multiply-commands
  (quote [[cpy b c]
          [inc a]
          [dec c]
          [jnz c -2]
          [dec d]
          [jnz d -5]]))

(defn multiply
  [state]
  (-> state
      (update-in [:registers 'a] + (* (get-in state [:registers 'b])
                                      (get-in state [:registers 'd])))
      (assoc-in [:registers 'c] 0)
      (assoc-in [:registers 'd] 0)
      (update :index + 6)))

(defn run-program
  {:test (fn []
           (is= (-> (run-program {:index     0
                                  :program   test-program
                                  :registers {'a 0}})
                    (:registers))
                {'a 3}))}
  [state]
  (let [command (get-in state [:program (:index state)])
        state
        (if (and (= (first command) 'cpy)
                 (= (subvec (:program state) (:index state) (min (+ 6 (:index state))
                                                                   (dec (count (:program state)))))
                    multiply-commands))
              (multiply state)

              (-> state
                  (run command)
                  (update :index inc)))]
    (if (< (:index state) (count (:program state)))
      (recur state)
      state)))

(comment
  (-> (run-program state)
      (get-in [:registers 'a]))

  (-> state
      (assoc-in [:registers 'a] 12)
      (run-program)
      (get-in [:registers 'a]))

  )