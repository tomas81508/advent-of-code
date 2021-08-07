(ns advent-of-code.dec-2015.day-07
  (:require [ysera.test :refer [is is-not is= deftest]]
            [clojure.string :refer [split-lines
                                    starts-with?]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2015/day_07_input.txt")
       (split-lines)))

; x AND y -> z
(def and-pattern (re-pattern "(\\w+) AND (\\w+) -> (\\w+)"))
(deftest and-pattern-test
         (is (re-matches and-pattern "x AND y -> d")))

; x OR y -> e
(def or-pattern (re-pattern "(\\w+) OR (\\w+) -> (\\w+)"))

; NOT e -> f
(def not-pattern (re-pattern "NOT (\\w+) -> (\\w+)"))

; p LSHIFT 2 -> q
(def lshift-pattern (re-pattern "(\\w+) LSHIFT (\\d+) -> (\\w+)"))

; p RSHIFT 2 -> q
(def rshift-pattern (re-pattern "(\\w+) RSHIFT (\\d+) -> (\\w+)"))

; 123 -> x
(def assign-pattern (re-pattern "(\\w+) -> (\\w+)"))

(defn run-instructions-in-order
  {:test (fn []
           (is= (run-instructions-in-order ["123 -> x"
                                            "456 -> y"
                                            "x AND y -> d"
                                            "x OR y -> e"
                                            "x LSHIFT 2 -> f"
                                            "y RSHIFT 2 -> g"
                                            "NOT x -> h"
                                            "NOT y -> i"])
                {"x" 123 "y" 456 "d" 72 "e" 507 "f" 492 "g" 114 "h" -124 "i" -457}))}
  [instructions]
  (reduce (fn [state instruction]
            (cond (re-matches not-pattern instruction)
                  (let [[_ input place] (re-matches not-pattern instruction)]
                    (assoc state place (bit-not (get state input))))

                  (re-matches and-pattern instruction)
                  (let [[_ input-1 input-2 place] (re-matches and-pattern instruction)]
                    (assoc state place (bit-and (get state input-1)
                                                (get state input-2))))

                  (re-matches or-pattern instruction)
                  (let [[_ input-1 input-2 place] (re-matches or-pattern instruction)]
                    (assoc state place (bit-or (get state input-1)
                                               (get state input-2))))

                  (re-matches lshift-pattern instruction)
                  (let [[_ input shifts place] (re-matches lshift-pattern instruction)]
                    (assoc state place (bit-shift-left (get state input) (read-string shifts))))

                  (re-matches rshift-pattern instruction)
                  (let [[_ input shifts place] (re-matches rshift-pattern instruction)]
                    (assoc state place (bit-shift-right (get state input) (read-string shifts))))

                  (re-matches assign-pattern instruction)
                  (let [[_ value place] (re-matches assign-pattern instruction)]
                    (if (re-matches (re-pattern "\\d+") value)
                      (assoc state place (read-string value))
                      (assoc state place (get state value))))

                  :else
                  (println "No match for" instruction)))
          {}
          instructions))


(defn- perform
  "Here a is the accumulator in reduce below"
  [a operation instruction place inputs]
  (if (contains? (:state a) place)
    a
    (let [handled-inputs (->> inputs
                              (map (fn [i]
                                     (if (re-matches #"\d+" i)
                                       (read-string i)
                                       (get-in a [:state i])))))]
      (if (some nil? handled-inputs)
        (update a :unhandled-instructions conj instruction)
        (assoc-in a [:state place] (apply operation handled-inputs))))))


(defn run-available-instructions
  {:test (fn []
           (is= (run-available-instructions {}
                                            ["123 -> x"
                                             "456 -> y"
                                             "x AND y -> d"
                                             "x OR y -> e"
                                             "x LSHIFT 2 -> f"
                                             "y RSHIFT 2 -> g"
                                             "NOT x -> h"
                                             "NOT y -> i"])
                {:state                  {"x" 123
                                          "y" 456
                                          "d" 72
                                          "e" 507
                                          "f" 492
                                          "g" 114
                                          "h" -124
                                          "i" -457}
                 :unhandled-instructions []})
           (is= (run-available-instructions {"i" 12}
                                            ["123 -> x"
                                             "456 -> y"
                                             "x AND y -> d"
                                             "x OR y -> e"
                                             "x LSHIFT 2 -> f"
                                             "y RSHIFT 2 -> g"
                                             "NOT x -> h"
                                             "NOT y -> i"])
                {:state                  {"x" 123
                                          "y" 456
                                          "d" 72
                                          "e" 507
                                          "f" 492
                                          "g" 114
                                          "h" -124
                                          "i" 12}
                 :unhandled-instructions []})
           (is= (run-available-instructions {}
                                            ["x AND y -> d"
                                             "NOT x -> h"
                                             "x OR y -> e"
                                             "456 -> y"
                                             "x LSHIFT 2 -> f"
                                             "NOT y -> i"
                                             "y RSHIFT 2 -> g"
                                             "123 -> x"])
                {:state                  {"y" 456
                                          "i" -457
                                          "g" 114
                                          "x" 123}
                 :unhandled-instructions ["x AND y -> d"
                                          "NOT x -> h"
                                          "x OR y -> e"
                                          "x LSHIFT 2 -> f"]}))}
  [state instructions]
  (reduce (fn [{state :state :as a} instruction]
            (cond (re-matches not-pattern instruction)
                  (let [[_ input place] (re-matches not-pattern instruction)]
                    (perform a bit-not instruction place [input]))

                  (re-matches and-pattern instruction)
                  (let [[_ input-1 input-2 place] (re-matches and-pattern instruction)]
                    (perform a bit-and instruction place [input-1 input-2]))

                  (re-matches or-pattern instruction)
                  (let [[_ input-1 input-2 place] (re-matches or-pattern instruction)]
                    (perform a bit-or instruction place [input-1 input-2]))

                  (re-matches lshift-pattern instruction)
                  (let [[_ input shifts place] (re-matches lshift-pattern instruction)]
                    (perform a bit-shift-left instruction place [input shifts]))

                  (re-matches rshift-pattern instruction)
                  (let [[_ input shifts place] (re-matches rshift-pattern instruction)]
                    (perform a bit-shift-right instruction place [input shifts]))

                  (re-matches assign-pattern instruction)
                  (let [[_ value place] (re-matches assign-pattern instruction)]
                    (perform a identity instruction place [value]))

                  :else
                  (println "No match for" instruction)))
          {:state                  state
           :unhandled-instructions []}
          instructions))


(defn run-instructions
  {:test (fn []
           (is= (run-instructions ["x AND y -> d"
                                   "NOT x -> h"
                                   "x OR y -> e"
                                   "456 -> y"
                                   "x LSHIFT 2 -> f"
                                   "NOT y -> i"
                                   "y RSHIFT 2 -> g"
                                   "123 -> x"])
                {"x" 123 "y" 456 "d" 72 "e" 507 "f" 492 "g" 114 "h" -124 "i" -457}))}
  ([instructions]
   (run-instructions instructions {}))
  ([instructions state]
   (loop [{state :state unhandled-instructions :unhandled-instructions}
          (run-available-instructions state instructions)]
     (cond (get state "a")
           state

           (empty? unhandled-instructions)
           state

           :else
           (let [{new-unhandled-instructions :unhandled-instructions
                  :as                        result}
                 (run-available-instructions state unhandled-instructions)]
             (if (= new-unhandled-instructions unhandled-instructions)
               (do (println "SORRY")
                   (println unhandled-instructions)
                   state)
               (recur result)))))))


(deftest puzzle-a
         (is= (-> (run-instructions (get-puzzle-input))
                  (get "a"))
              3176))

(deftest puzzle-b
         (is= (-> (run-instructions (get-puzzle-input) {"b" 3176})
                  (get "a"))
              14710))
