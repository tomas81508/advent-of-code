(ns advent-of-code.dec-2022.day-21
  (:require [ysera.test :refer [is is-not is=]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_21_input.txt")
                (clojure.string/split-lines)))

(def test-input ["root: pppw + sjmn"
                 "dbpl: 5"
                 "cczh: sllz + lgvd"
                 "zczc: 2"
                 "ptdq: humn - dvpt"
                 "dvpt: 3"
                 "lfqf: 4"
                 "humn: 5"
                 "ljgn: 2"
                 "sjmn: drzm * dbpl"
                 "sllz: 4"
                 "pppw: cczh / lfqf"
                 "lgvd: ljgn * ptdq"
                 "drzm: hmdt - zczc"
                 "hmdt: 32"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {"sllz"   4
                 "dvpt"   3
                 "lfqf"   4
                 "humn"   5
                 "ljgn"   2
                 "dbpl"   5
                 "hmdt"   32
                 "zczc"   2
                 :waiting {"root" {:operation "+" :monkeys ["pppw" "sjmn"]}
                           "cczh" {:operation "+" :monkeys ["sllz" "lgvd"]}
                           "ptdq" {:operation "-" :monkeys ["humn" "dvpt"]}
                           "sjmn" {:operation "*" :monkeys ["drzm" "dbpl"]}
                           "pppw" {:operation "/" :monkeys ["cczh" "lfqf"]}
                           "lgvd" {:operation "*" :monkeys ["ljgn" "ptdq"]}
                           "drzm" {:operation "-" :monkeys ["hmdt" "zczc"]}}}))}
  [input]
  (reduce (fn [a v]
            (let [[_ monkey-id yell] (re-find #"(\w{4}): (.*)" v)]
              (if-let [n (re-find #"^\d+$" yell)]
                (assoc a monkey-id (read-string n))
                (let [[_ monkey-id1 operation monkey-id2] (re-find #"(\w{4}) (.) (\w{4})" yell)]
                  (assoc-in a [:waiting monkey-id] {:operation operation :monkeys [monkey-id1 monkey-id2]})))))
          {}
          input))

(defn calculate-numbers
  {:test (fn []
           (is= (-> (create-state test-input)
                    (calculate-numbers)
                    (get "root"))
                152))}
  [state]
  (loop [state state]
    (let [next-state (reduce-kv (fn [state k v]
                                  (let [[id1 id2] (:monkeys v)]
                                    (if (not (and (contains? state id1) (contains? state id2)))
                                      state
                                      (let [v1 (get state id1)
                                            v2 (get state id2)
                                            value ((eval (read-string (:operation v))) v1 v2)]
                                        (-> state
                                            (assoc k value)
                                            (update :waiting dissoc k))))))
                                state
                                (:waiting state))]
      (if (= (keys next-state) (keys state))
        state
        (recur next-state)))))

(comment
  (time (-> (create-state input)
            (calculate-numbers)
            (get "root")))
  )

(defn create-state-2
  [input]
  (-> (create-state input)
      (dissoc "humn")
      (assoc-in [:waiting "root" :operation] "=")
      (assoc-in [:waiting "humn"] "x")))


(defn find-start-number
  {:test (fn []
           (is= (-> (create-state-2 test-input)
                    (find-start-number)
                    (get "humn"))
                301))}
  [state]
  (let [solve-v2 (fn [operation v v1]
                   (condp = operation
                     ; v = v1 / v2 => v2 = v1 / v
                     "/" (/ v1 v)
                     ; v = v1 * v2 => v2 = v / v1
                     "*" (/ v v1)
                     ; v = v1 + v2 => v2 = v - v1
                     "+" (- v v1)
                     ; v = v1 - v2 => v2 = v1 - v
                     "-" (- v1 v)))
        solve-v1 (fn [operation v v2]
                   (condp = operation
                     ; v = v1 / v2 => v1 = v * v2
                     "/" (* v v2)
                     ; v = v1 * v2 => v1 = v / v2
                     "*" (/ v v2)
                     ; v = v1 + v2 => v1 = v - v2
                     "+" (- v v2)
                     ; v = v1 - v2 => v1 = v + v2
                     "-" (+ v v2)))]
    (loop [state state]
      (let [next-state
            (reduce-kv (fn [state k v]
                         (let [[id1 id2] (:monkeys v)]
                           (cond (and (contains? state id1) (contains? state id2))
                                 (let [v1 (get state id1)
                                       v2 (get state id2)
                                       value ((eval (read-string (:operation v))) v1 v2)]
                                   (-> state
                                       (assoc k value)
                                       (update :waiting dissoc k)))

                                 (and (contains? state id1) (contains? state k))
                                 (let [v1 (get state id1)
                                       v0 (get state k)
                                       value2 (solve-v2 (:operation v) v0 v1)]
                                   (-> state
                                       (assoc id2 value2)
                                       (update :waiting dissoc k)))

                                 (and (contains? state id2) (contains? state k))
                                 (let [v2 (get state id2)
                                       v0 (get state k)
                                       value1 (solve-v1 (:operation v) v0 v2)]
                                   (-> state
                                       (assoc id1 value1)
                                       (update :waiting dissoc k)))

                                 (and (= (:operation v) "=")
                                      (contains? state id1))
                                 (let [value (get state id1)]
                                   (-> state
                                       (assoc id2 value)
                                       (update :waiting dissoc k)))

                                 (and (= (:operation v) "=")
                                      (contains? state id2))
                                 (let [value (get state id2)]
                                   (-> state
                                       (assoc id1 value)
                                       (update :waiting dissoc k)))

                                 :else
                                 state
                                 )))
                       state
                       (:waiting state))]
        (if (= (keys next-state) (keys state))
          state
          (recur next-state))))))

(comment
  (time (-> (create-state-2 input)
            (find-start-number)
            (get "humn")))
  ; "Elapsed time: 37.686226 msecs"
  ; => 3952288690726
  )


