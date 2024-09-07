(ns advent-of-code.dec-2017.day-08
  (:require [ysera.test :refer [is=]]))

(def test-input ["b inc 5 if a > 1"
                 "a inc 1 if b < 5"
                 "c dec -10 if a >= 1"
                 "c inc -20 if c == 10"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {"a" 0 "b" 0 "c" 0}))}
  [input]
  (->> input
       (map (fn [line] (first (clojure.string/split line #" "))))
       (reduce (fn [a v] (assoc a v 0))
               {})))

(def test-state (create-state test-input))

(def parse-instruction #"([^ ]+) ([^ ]+) ([^ ]+) if ([^ ]+) ([^ ]+) ([^ ]+)")

(defn translate-operation [operation]
  (condp = operation
    "==" "="
    "!=" "not="
    operation))

(defn run-instruction
  [state instruction]
  (let [[_ register operation value pred-register pred-operation pred-value] (re-find parse-instruction instruction)
        pred-register-value (get state pred-register)
        translated-operation (translate-operation pred-operation)
        pred-expression (read-string (str "(" (clojure.string/join " " [translated-operation pred-register-value pred-value]) ")"))]
    (if-not (eval pred-expression)
      state
      (update state register (if (= operation "inc") + -) (read-string value)))))

(defn run-instructions
  [state input]
  (reduce run-instruction
          state
          input))

(def input (->> (slurp "src/advent_of_code/dec_2017/day_08_input.txt")
                (clojure.string/split-lines)))

(def state (create-state input))

(comment
  (run-instructions test-state test-input)
  (->> (run-instructions state input)
       (vals)
       (reduce max))
  )

(defn run-controlled-instructions
  [state instructions]
  (reduce (fn [{state :state highest-value :highest-value} instruction]
            (let [state (run-instruction state instruction)
                  max-value (apply max (vals state))]
              {:state state :highest-value (max highest-value max-value)}))
          {:state         state
           :highest-value 0}
          instructions))

(comment
  (run-controlled-instructions state input)
  )



