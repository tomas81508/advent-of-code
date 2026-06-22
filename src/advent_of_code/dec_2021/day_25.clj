(ns advent-of-code.dec-2021.day-25
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [advent-of-code.math :refer [floor ceil]]))

(def input (->> (slurp "src/advent_of_code/dec_2021/day_25_input.txt")
                (string/split-lines)))

(def test-input ["v...>>.vv>"
                 ".vv>>.vv.."
                 ">>.>v>...v"
                 ">>v>>.>.v."
                 "v>v.vv.v.."
                 ">.>>..v..."
                 ".vv..>.>v."
                 "v.v..>>v.v"
                 "....v..v.>"])

(defn create-state
  {:test (fn []
           (is= (create-state ".>v"
                              "..>")
                {:right  #{[1 0] [2 1]}
                 :south  #{[2 0]}
                 :width  3
                 :height 2}))}
  [& input-rows]
  (->> (vec input-rows)
       (reduce-kv (fn [a y row]
                    (->> (vec row)
                         (reduce-kv (fn [a x c]
                                      (cond (= c \>) (update a :right conj [x y])
                                            (= c \v) (update a :south conj [x y])
                                            :else a))
                                    a)))
                  {:right  #{}
                   :south  #{}
                   :width  (count (first input-rows))
                   :height (count input-rows)})))

(defn occupied?
  {:test (fn []
           (is (occupied? (create-state ">.") [0 0]))
           (is-not (occupied? (create-state ">.") [1 0])))}
  [state position]
  (or (contains? (:right state) position)
      (contains? (:south state) position)))

(defn move-direction
  [state direction]
  (let [d ({:right [1 0] :south [0 1]} direction)
        modify-out-of-bound (fn [p] (cond (>= (first p) (:width state))
                                          (assoc p 0 0)
                                          (>= (second p) (:height state))
                                          (assoc p 1 0)
                                          :else p))]
    (update state direction
            (fn [positions]
              (->> positions
                   (reduce (fn [new-positions position]
                             (let [maybe-new-position (-> (mapv + d position)
                                                          (modify-out-of-bound))]
                               (if (occupied? state maybe-new-position)
                                 (conj new-positions position)
                                 (conj new-positions maybe-new-position))))
                           #{}))))))

(defn step
  {:test (fn []
           (is= (-> (create-state "...>>>>>...")
                    (step))
                (create-state "...>>>>.>.."))
           (is= (-> (create-state "...>>>>.>..")
                    (step))
                (create-state "...>>>.>.>."))
           (is= (-> (create-state ">v....v.."
                                  "......>..")
                    (step))
                (create-state ">........"
                              ".v....v>."))
           (is= (-> (create-state "..>"
                                  ".v.")
                    (step))
                (create-state ">v."
                              "...")))}
  [state]
  (-> state
      (move-direction :right)
      (move-direction :south)))

(defn stabilize
  {:test (fn []
           (is= (-> (apply create-state test-input)
                    (stabilize))
                58))}
  [state]
  (loop [state state
         i 1]
    (let [new-state (step state)]
      (cond (= new-state state) i
            :else (recur new-state (inc i))))))

(comment
  (time (-> (apply create-state input)
            (stabilize)))
  ; "Elapsed time: 2379.849916 msecs"
  ; => 482
  )
