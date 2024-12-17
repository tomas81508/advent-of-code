(ns advent-of-code.dec-2024.day-17
  (:require [clojure.math :refer [pow]]
            [advent-of-code.test :refer [is is-not is=]]))

(def state {:registers {:A 32916674
                        :B 0
                        :C 0}
            :pointer   0
            :output    []
            :program   [2 4 1 1 7 5 0 3 1 4 4 0 5 5 3 0]})

(def test-state {:registers {:A 729
                             :B 0
                             :C 0}
                 :pointer   0
                 :output    []
                 :program   [0 1 5 4 3 0]})

(defn get-combo-operand
  [state operand]
  (condp = operand
    4 (get-in state [:registers :A])
    5 (get-in state [:registers :B])
    6 (get-in state [:registers :C])
    operand))

(defn adv
  {:test (fn []
           (is= (adv test-state 2)
                (assoc-in test-state [:registers :A] 182)))}
  [state operand]
  (update-in state [:registers :A] (fn [A] (long (/ A (pow 2 (get-combo-operand state operand)))))))

(defn bxl
  {:test (fn []
           (is= (bxl {:registers {:B 5}} 4)
                {:registers {:B 1}}))}
  [state operand]
  (update-in state [:registers :B] (fn [B] (bit-xor B operand))))

(defn bst
  {:test (fn []
           (is= (bst {:registers {:B 5}} 2)
                {:registers {:B 2}})
           (is= (bst {:registers {:B 5}} 16)
                {:registers {:B 0}}))}
  [state operand]
  (assoc-in state [:registers :B] (mod (get-combo-operand state operand) 8)))

(defn jnz
  {:test (fn []
           (is= (jnz {:registers {:A 0}
                      :pointer   2}
                     10)
                {:registers {:A 0}
                 :pointer   4})
           (is= (jnz {:registers {:A 2}
                      :pointer   2}
                     10)
                {:registers {:A 2}
                 :pointer   10}))}
  [state operand]
  (if (zero? (get-in state [:registers :A]))
    (update state :pointer + 2)
    (assoc state :pointer operand)))

(defn bxc
  {:test (fn []
           (is= (bxc {:registers {:B 5
                                  :C 4}} "ignored")
                {:registers {:B 1
                             :C 4}}))}
  [state _]
  (update-in state [:registers :B] bit-xor (get-in state [:registers :C])))

(defn out
  {:test (fn []
           (is= (out {:output [1 2]} 3)
                {:output [1 2 3]}))}
  [state operand]
  (update state :output conj (mod (get-combo-operand state operand) 8)))

(defn bdv
  {:test (fn []
           (is= (bdv {:registers {:A 729
                                  :B 0}} 2)
                {:registers {:A 729
                             :B 182}}))}
  [state operand]
  (assoc-in state [:registers :B] (long (/ (get-in state [:registers :A])
                                           (pow 2 (get-combo-operand state operand))))))

(defn cdv
  {:test (fn []
           (is= (cdv {:registers {:A 729
                                  :C 0}} 2)
                {:registers {:A 729
                             :C 182}}))}
  [state operand]
  (assoc-in state [:registers :C] (long (/ (get-in state [:registers :A])
                                           (pow 2 (get-combo-operand state operand))))))

(defn do-one-instruction
  [state]
  (let [operator (nth (:program state) (:pointer state) nil)
        operand (nth (:program state) (inc (:pointer state)) nil)]
    (if (nil? operator)
      state
      (case operator
        0 (-> (adv state operand)
              (update :pointer + 2))
        1 (-> (bxl state operand)
              (update :pointer + 2))
        2 (-> (bst state operand)
              (update :pointer + 2))
        3 (jnz state operand)
        4 (-> (bxc state operand)
              (update :pointer + 2))
        5 (-> (out state operand)
              (update :pointer + 2))
        6 (-> (bdv state operand)
              (update :pointer + 2))
        7 (-> (cdv state operand)
              (update :pointer + 2))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-state) "4,6,3,5,6,3,5,2,1,0"))}
  [state]
  (loop [state state]
    (let [next-state (do-one-instruction state)]
      (if (= next-state state)
        (clojure.string/join "," (:output state))
        (recur next-state)))))

(def test-state-2 {:registers {:A "ignored"
                               :B 0
                               :C 0}
                   :pointer   0
                   :output    []
                   :program   [0 3 5 4 3 0]})

(defn compare-programs
  {:test (fn []
           (is (compare-programs [1 2 3] [1 2]))
           (is-not (compare-programs [1 2 3] [2 2 3])))}
  [goal-program program]
  (every? true? (map = goal-program program)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-state-2) 117440))}
  [state]
  (let [program (:program state)]
    (loop [candidates (range 8)
           number-of-numbers-to-check 1]
      (let [partial-program (take-last number-of-numbers-to-check program)
            working-candidates (reduce (fn [working-candidates candidate]
                                         (let [output (loop [state (assoc-in state [:registers :A] candidate)]
                                                        (let [next-state (do-one-instruction state)]
                                                          (if (= next-state state)
                                                            (:output state)
                                                            (recur next-state))))]
                                           (if (= output partial-program)
                                             (conj working-candidates candidate)
                                             working-candidates)))
                                       []
                                       candidates)]
        (if (= number-of-numbers-to-check (count program))
          (first working-candidates)
          (recur (reduce (fn [new-candidates working-candidate]
                           (reduce conj new-candidates (->> (range 8)
                                                            (map (fn [i] (+ i (* working-candidate 8)))))))
                         []
                         working-candidates)
                 (inc number-of-numbers-to-check)))))))

(comment
  (time (part-1 state))
  ;; "Elapsed time: 0.259833 msecs"
  ;=> "3,4,3,1,7,6,5,6,0"

  (time (part-2 state))
  ;; "Elapsed time: 43.436667 msecs"
  ;=> 109019930331546
  )



