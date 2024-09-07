(ns advent-of-code.dec-2015.day-13
  (:require [ysera.test :refer [is= deftest]]
            [clojure.math.combinatorics :as combinatorics]))

(def input (-> (slurp "src/advent_of_code/dec_2015/day_13_input.txt")
               (clojure.string/split-lines)))

(def test-input ["Alice would gain 54 happiness units by sitting next to Bob."
                 "Alice would lose 79 happiness units by sitting next to Carol."
                 "Alice would lose 2 happiness units by sitting next to David."
                 "Bob would gain 83 happiness units by sitting next to Alice."
                 "Bob would lose 7 happiness units by sitting next to Carol."
                 "Bob would lose 63 happiness units by sitting next to David."
                 "Carol would lose 62 happiness units by sitting next to Alice."
                 "Carol would gain 60 happiness units by sitting next to Bob."
                 "Carol would gain 55 happiness units by sitting next to David."
                 "David would gain 46 happiness units by sitting next to Alice."
                 "David would lose 7 happiness units by sitting next to Bob."
                 "David would gain 41 happiness units by sitting next to Carol."])

(def line-pattern #"([^ ]+) would ([^ ]+) ([^ ]+) happiness units by sitting next to ([^ ]+)\.")

(defn parse-line
  {:test (fn []
           (is= (parse-line "Alice would gain 54 happiness units by sitting next to Bob.")
                ["Alice" "Bob" 54])
           (is= (parse-line "Alice would lose 79 happiness units by sitting next to Carol.")
                ["Alice" "Carol" -79]))}
  [text]
  (let [[_ name1 sign value-string name2] (re-find line-pattern text)
        value (read-string value-string)]
    [name1 name2 (if (= sign "gain") value (- value))]))

(defn create-state
  [input]
  (->> input
       (reduce (fn [a line]
                 (let [[name1 name2 value] (parse-line line)]
                   (assoc-in a [name1 name2] value)))
               {})))

(def test-state (create-state test-input))

(def state (create-state input))

(defn happiness-for-arrangement
  {:test (fn []
           (is= (happiness-for-arrangement test-state ["David" "Alice" "Bob" "Carol"])
                330))}
  [state arrangement]
  (let [circle (cons (last arrangement) arrangement)]
    (->> circle
         (partition 2 1)
         (reduce (fn [a [name1 name2]]
                   (+ a
                      (get-in state [name1 name2])
                      (get-in state [name2 name1])))
                 0))))

(defn happiness
  {:test (fn []
           (is= (happiness test-state)
                330))}
  [state]
  (->> (combinatorics/permutations (keys state))
       (map (fn [arrangement] (happiness-for-arrangement state arrangement)))
       (reduce max 0)))

(comment
  (happiness state)
  664
  )

; Part 2 -- Adding yourself

(defn add-ourself-to-state
  [state]
  (-> (reduce (fn [state name]
                (assoc-in state [name "Me"] 0))
              state
              (keys state))
      (assoc "Me" (zipmap (keys state) (repeat 0)))))

(def part2-state (add-ourself-to-state state))

(comment
  (happiness part2-state)
  640
  )






