(ns advent-of-code.dec-2021.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def puzzle-input (->> (slurp "src/advent_of_code/dec_2021/day_10_input.txt")
                       (clojure.string/split-lines)))

(def test-input ["[({(<(())[]>[[{[]{<()<>>"
                 "[(()[<>])]({[<{<<[]>>("
                 "{([(<{}[<>[]}>{[]{[(<()>"
                 "(((({<>}<{<{<>}{[]{[]{}"
                 "[[<[([]))<([[{}[[()]]]"
                 "[{[{({}]{}}([{[{{{}}([]"
                 "{<[[]]>}<{[{[{[]{()[[[]"
                 "[<(<(<(<{}))><([]([]()"
                 "<{([([[(<>()){}]>(<<{{"
                 "<{([{{}}[<[[[<>{}]]]>[]]"])

(def start-symbols #{\( \{ \[ \<})

(def matching-symbol {\( \) \{ \} \[ \] \< \>})

(defn test-chunk
  {:test (fn []
           (is= (test-chunk "([") {:status :incomplete :stack [\[ \(]})
           (is= (test-chunk "([])") :valid)
           (is= (test-chunk "{()()()}") :valid)
           (is= (test-chunk "<([{}])>") :valid)
           (is= (test-chunk "[<>({}){}[([])<>]]") :valid)
           (is= (test-chunk "(((((((((())))))))))") :valid)
           (is= (test-chunk "{([(<{}[<>[]}>{[]{[(<()>")
                {:message "Expected ], but found } instead." :invalid-symbol \}})
           (is= (test-chunk "[[<[([]))<([[{}[[()]]]")
                {:message "Expected ], but found ) instead." :invalid-symbol \)})
           (is= (test-chunk "[{[{({}]{}}([{[{{{}}([]")
                {:message "Expected ), but found ] instead." :invalid-symbol \]})
           (is= (test-chunk "[<(<(<(<{}))><([]([]()")
                {:message "Expected >, but found ) instead." :invalid-symbol \)})
           (is= (test-chunk "<{([([[(<>()){}]>(<<{{")
                {:message "Expected ], but found > instead." :invalid-symbol \>}))}
  [chunk]
  (loop [stack (list)
         [c & cs] chunk]
    (cond (nil? c)
          (if (empty? stack)
            :valid
            {:status :incomplete
             :stack  stack})

          (contains? start-symbols c)
          (recur (conj stack c) cs)

          :else
          (let [[h & t] stack]
            (if (= (matching-symbol h) c)
              (recur t cs)
              {:message        (str "Expected " (matching-symbol h) ", but found " c " instead.")
               :invalid-symbol c})))))

(defn calculate-score-a
  {:test (fn []
           (is= (calculate-score-a \)) 3)
           (is= (calculate-score-a \]) 57)
           (is= (calculate-score-a \}) 1197)
           (is= (calculate-score-a \>) 25137))}
  [symbol]
  ({\) 3 \] 57 \} 1197 \> 25137} symbol))

(defn solver-a
  {:test (fn []
           (is= (solver-a test-input)
                26397))}
  [input]
  (->> input
       (map test-chunk)
       (remove (fn [{status :status}] (= status :incomplete)))
       (map :invalid-symbol)
       (map calculate-score-a)
       (reduce +)))

(comment
  (time (solver-a puzzle-input))
  ; "Elapsed time: 1.977131 msecs"
  266301
  )


(defn calculate-score-b
  {:test (fn []
           (is= (calculate-score-b \)) 1)
           (is= (calculate-score-b \]) 2)
           (is= (calculate-score-b \}) 3)
           (is= (calculate-score-b \>) 4))}
  [symbol]
  ({\) 1 \] 2 \} 3 \> 4} symbol))


(defn score
  {:test (fn []
           (is= (score [\] \) \} \>])
                294))}
  [symbols]
  (loop [[s & ss] symbols
         score 0]
    (if s
      (recur ss (+ (* 5 score) (calculate-score-b s)))
      score)))

(comment
  (time (let [sorted-score (->> puzzle-input
                                (map test-chunk)
                                (filter (fn [{status :status}] (= status :incomplete)))
                                (map :stack)
                                (map (fn [row] (map matching-symbol row)))
                                (map score)
                                (sort)
                                (into []))]
          (get sorted-score (int (/ (count sorted-score) 2)))))
  ; "Elapsed time: 3.104637 msecs"
  3404870164
  )

