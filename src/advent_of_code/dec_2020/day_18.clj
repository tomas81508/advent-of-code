(ns advent-of-code.dec-2020.day-18
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.collections :refer [seq-contains?]]
            [clojure.string :refer [ends-with? split starts-with?]]))

(defn get-puzzle-input []
  (-> (slurp "src/advent_of_code/dec_2020/day_18.txt")
      (clojure.string/split-lines)))

(defn split-expression
  {:test (fn []
           (is= (split-expression "(2 + (4 * 19)) * 2")
                ["(" "2" "+" "(" "4" "*" "19" ")" ")" "*" "2"])
           (is= (split-expression "(7 * 8)")
                ["(" "7" "*" "8" ")"]))}
  [expression]
  (-> expression
      (clojure.string/replace "(" "( ")
      (clojure.string/replace ")" " )")
      (split #" ")))


(defn split-first-thing
  {:test (fn []
           (is= (split-first-thing "")
                [[""] []])
           (is= (split-first-thing "2 + 4")
                [["2"] ["+" "4"]])
           (is= (split-first-thing "+ 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
                [["+"] ["4" "*" "9" ")" "*" "(" "6" "+" "9" "*" "8" "+" "6" ")" "+" "6" ")" "+" "2" "+" "4" "*" "2"]])
           (is= (split-first-thing "(7 * 8)")
                [["7" "*" "8"] []])
           (is= (split-first-thing "(2 + 4 * 9) * 2")
                [["2" "+" "4" "*" "9"] ["*" "2"]])
           (is= (split-first-thing "(2 + (4 * 9)) * 2")
                [["2" "+" "(" "4" "*" "9" ")"] ["*" "2"]])
           (is= (split-first-thing "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
                [["(" "2" "+" "4" "*" "9" ")" "*" "(" "6" "+" "9" "*" "8" "+" "6" ")" "+" "6"] ["+" "2" "+" "4" "*" "2"]]))}
  [expression]
  (let [expression (if (string? expression)
                     (split-expression expression)
                     expression)]
    (loop [[f & r :as expression] expression
           result []
           parenthesis 0
           index 0]
      (cond (and (zero? parenthesis) (pos? index))
            [result (vec expression)]

            (and (= f "(") (zero? index))
            (recur r [] (inc parenthesis) (inc index))

            (= f "(")
            (recur r (conj result f) (inc parenthesis) (inc index))

            (and (= f ")") (= parenthesis 1))
            [result (vec r)]

            (= f ")")
            (recur r (conj result f) (dec parenthesis) (inc index))

            :else
            (recur r (conj result f) parenthesis (inc index))))))

(split-first-thing (split-expression "(7 * 8)"))

(defn evaluate
  {:test (fn []
           (is= (evaluate "2 * 3") 6)
           (is= (evaluate "2 + 3 * 6") 30)
           (is= (evaluate "(7 * 8)") 56)
           (is= (evaluate "2 * 3 + (4 * 5)") 26)
           (is= (evaluate "5 + (8 * 3 + 9 + 3 * 4 * 3)") 437)
           (is= (evaluate "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 12240)
           (is= (evaluate "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 13632))}
  [expression]
  (loop [expression (if (string? expression) (split-expression expression) expression)
         result {:left nil :operation nil}]
    (if (empty? expression)
      (:left result)
      (let [[first-thing rest-of-things] (split-first-thing expression)
            first-thing-value (if (and (= (count first-thing) 1)
                                       (not (coll? (first first-thing))))
                                (first first-thing)
                                (str (evaluate first-thing)))]
        (cond (= first-thing-value "*")
              (recur rest-of-things (assoc result :operation *))

              (= first-thing-value "+")
              (recur rest-of-things (assoc result :operation +))

              (nil? (:left result))
              (recur rest-of-things (assoc result :left (read-string first-thing-value)))

              :else
              (recur rest-of-things
                     (assoc result :left ((:operation result)
                                          (:left result)
                                          (read-string first-thing-value))
                                   :operation nil)))))))

(deftest puzzle-a
  (is= (time (->> (get-puzzle-input)
                  (map evaluate)
                  (apply +)))
       ; "Elapsed time: 16.379615 msecs"
       280014646144))


(defn s-number?
  {:test (fn []
           (is (s-number? "33"))
           (is-not (s-number? "+")))}
  [s]
  {:pre [(string? s)]}
  (re-matches #"[\d]+" s))

(defn create-output
  "Using Shunting-yard algorithm"
  {:test (fn []
           (is= (create-output "2 + 2")
                ["2" "2" "+"])
           (is= (create-output "3 * 2 + 1")
                ["3" "2" "1" "+" "*"])
           (is= (create-output "2 + 3 * 4")
                ["2" "3" "+" "4" "*"])
           (is= (create-output "1 + 2 * 3 + 4 * 5 + 6")
                ["1" "2" "+" "3" "4" "+" "*" "5" "6" "+" "*"])
           (is= (create-output "2 * 3 + (4 * 5)")
                ["2" "3" "4" "5" "*" "+" "*"])
           (is= (create-output "5 + (8 * 3 + 9 + 3 * 4 * 3)")
                ["5" "8" "3" "9" "+" "3" "+" "*" "4" "*" "3" "*" "+"]))}
  [input]
  (let [rpn (reduce (fn [a v]
                      (cond (s-number? v)
                            (update a :output conj v)

                            (= v "(")
                            (update a :operators conj v)

                            (= v ")")
                            (loop [a a]
                              (let [op (first (:operators a))]
                                (if (= op "(")
                                  (update a :operators (partial drop 1))
                                  (recur (-> a
                                             (update :output conj op)
                                             (update :operators (partial drop 1)))))))

                            (= v "+")
                            (as-> a $
                                  (loop [a a]
                                    (let [operator (first (:operators a))]
                                      (if (= operator "+")
                                        (recur (-> a
                                                   (update :output conj operator)
                                                   (update :operators (partial drop 1))))
                                        a)))
                                  (update $ :operators conj "+"))

                            (= v "*")
                            (as-> a $
                                  (loop [a a]
                                    (let [operator (first (:operators a))]
                                      (if (or (= operator "*") (= operator "+"))
                                        (recur (-> a
                                                   (update :output conj operator)
                                                   (update :operators (partial drop 1))))
                                        a)))
                                  (update $ :operators conj "*"))))
                    {:output    []
                     :operators (list)}
                    (split-expression input))]
    (concat (:output rpn) (:operators rpn))))

(defn evaluate-2
  {:test (fn []
           (is= (evaluate-2 "1 + 2 + 3") 6)
           (is= (evaluate-2 "1 + 2 * 3 + 4 * 5 + 6") 231)
           (is= (evaluate-2 "2 * 3 + (4 * 5)") 46)
           (is= (evaluate-2 "5 + (8 * 3 + 9 + 3 * 4 * 3)") 1445)
           (is= (evaluate-2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") 669060)
           (is= (evaluate-2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") 23340))}
  [input]
  (let [rpn-expression (create-output input)]
    (loop [[f & r] rpn-expression
           memory-stack (list)]
      (if-not f
        (read-string (first memory-stack))
        (cond (s-number? f)
              (recur r (conj memory-stack f))

              (or (= f "*") (= f "+"))
              (recur r
                     (let [[n1 n2 & stack-rest] memory-stack
                           operation (if (= f "+") + *)]
                       (conj stack-rest (str (operation (read-string n1)
                                                        (read-string n2)))))))))))


(deftest puzzle-b
  (is= (time (->> (get-puzzle-input)
                  (map evaluate-2)
                  (apply +)))
       ; "Elapsed time: 14.584276 msecs"
       9966990988262))





















