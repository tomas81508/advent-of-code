(ns advent-of-code.dec-2020.day-08
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_08.txt")
       (clojure.string/split-lines)))

(def test-input ["nop +0"
                 "acc +1"
                 "jmp +4"
                 "acc +3"
                 "jmp -3"
                 "acc -99"
                 "acc +1"
                 "jmp -4"
                 "acc +6"])

(defn parse-instruction
  {:test (fn []
           (is= (parse-instruction "acc +1")
                {:operation "acc" :argument 1})
           (is= (parse-instruction "nop +0")
                {:operation "nop" :argument 0})
           (is= (parse-instruction "jmp -4")
                {:operation "jmp" :argument -4}))}
  [instruction]
  (let [[_ operation argument] (re-matches (re-pattern "([\\w]{3}) ([+|-][\\d]+)")
                                           instruction)]
    {:operation operation
     :argument  (read-string argument)}))

(defn create-program
  [input]
  {:accumulator   0
   :current-index 0
   :counter       0
   :instructions  (->> input
                       (mapv parse-instruction))})

(defmulti run-instruction (fn [instruction _] (:operation instruction)))

(defmethod run-instruction "acc"
  [instruction program]
  {:accumulator   (+ (:accumulator program) (:argument instruction))
   :current-index (inc (:current-index program))})

(defmethod run-instruction "nop"
  [instruction program]
  {:current-index (inc (:current-index program))})

(defmethod run-instruction "jmp"
  [instruction program]
  {:current-index (+ (:current-index program) (:argument instruction))})


(def test-input-2 ["nop +0"
                   "acc +1"
                   "jmp +4"
                   "acc +3"
                   "jmp -3"
                   "acc -99"
                   "acc +1"
                   "nop -4"
                   "acc +6"])


(defn run-program-until-loop
  {:test (fn []
           (is= (-> test-input
                    (create-program)
                    (run-program-until-loop)
                    (:accumulator))
                5)
           (is= (-> test-input-2
                    (create-program)
                    (run-program-until-loop)
                    (:accumulator))
                8))}
  [program]
  (let [current-index (:current-index program)
        current-instruction (get (:instructions program) current-index)]
    (cond (not current-instruction)
          (assoc program :terminated true)

          (:executed current-instruction)
          program

          :else
          (let [{new-accumulator :accumulator new-current-index :current-index}
                (run-instruction current-instruction program)]
            (recur (cond-> (-> program
                               (update :counter inc)
                               (assoc-in [:instructions current-index :executed] true))
                           new-accumulator (assoc :accumulator new-accumulator)
                           new-current-index (assoc :current-index new-current-index)))))))


(deftest puzzle-a
         (is= (-> (get-puzzle-input)
                  (create-program)
                  (run-program-until-loop)
                  (:accumulator))
              1475))

(defn create-program-candidates
  {:test (fn []
           (is= (->> (create-program test-input)
                     (create-program-candidates)
                     (count))
                5))}
  [program]
  (reduce (fn [programs index]
            (let [instruction-at-index (nth (:instructions program) index)]
              (cond (= (:operation instruction-at-index) "jmp")
                    (conj programs (assoc-in program [:instructions index :operation] "nop"))

                    (= (:operation instruction-at-index) "nop")
                    (conj programs (assoc-in program [:instructions index :operation] "jmp"))

                    :else
                    programs)))
          [program]
          (range (count (:instructions program)))))

(defn find-correct-program
  {:test (fn []
           (is= (-> test-input
                    (create-program)
                    (create-program-candidates)
                    (find-correct-program)
                    (:terminated))
                true))}
  [programs]
  (->> programs
       (map run-program-until-loop)
       (filter :terminated)
       (first)))

(deftest puzzle-b
         (is= (-> (get-puzzle-input)
                  (create-program)
                  (create-program-candidates)
                  (find-correct-program)
                  (:accumulator))
              1270))




