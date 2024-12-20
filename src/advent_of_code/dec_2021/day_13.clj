(ns advent-of-code.dec-2021.day-13
  (:require [advent-of-code.test :refer [is=]]))

(def test-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")
(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_13_input.txt"))

(defn create-state
  [input]
  (let [[cells-input instructions-input] (-> input
                                             (clojure.string/split #"\n\n"))
        cells (->> cells-input
                   (clojure.string/split-lines)
                   (map (fn [l] (->> (clojure.string/split l #",")
                                     (mapv read-string)))))
        instructions (clojure.string/split-lines instructions-input)]
    {:cells        (set cells)
     :instructions instructions}))

(def test-state (create-state test-input))
(def puzzle-state (create-state puzzle-input))

(defn fold
  {:test (fn []
           (is= (fold (:cells test-state) "fold along y=7")
                #{[8 4] [3 4] [3 0] [9 0] [0 1] [0 0] [1 4] [4 3] [4 1] [0 3] [10 2] [10 4] [6 4] [6 2] [2 0] [9 4] [6 0]}))}
  [cells instruction]
  (let [d (.charAt instruction 11)
        v (read-string (subs instruction 13))]
    (->> cells
         (map (fn [[x y :as c]]
                (cond (and (= d \y) (> y v))
                      [x (- v (- y v))]

                      (and (= d \x) (> x v))
                      [(- v (- x v)) y]

                      :else c)))
         (set))))

(defn fold-instructions
  {:test (fn []
           (is= (fold-instructions (:cells test-state) (:instructions test-state))
                #{[4 3] [0 0] [1 0] [3 4] [4 2] [3 0] [4 1] [1 4] [0 3] [2 4] [0 2] [2 0] [0 4] [4 4] [0 1] [4 0]}))}
  [cells instructions]
  (reduce fold
          cells
          instructions))

(comment
  (time (count (fold (:cells puzzle-state) (first (:instructions puzzle-state)))))
  ; "Elapsed time: 1.59524 msecs"
  759

  (time (let [answer (fold-instructions (:cells puzzle-state) (:instructions puzzle-state))
              max-x (->> (map first answer)
                         (reduce max))
              max-y (->> (map second answer)
                         (reduce max))]
          (->> (range (inc max-y))
               (map (fn [y]
                      (->> (range (inc max-x))
                           (map (fn [x]
                                  (if (contains? answer [x y]) "#" " ")))
                           (apply str)))))))
  ; "Elapsed time: 4.075722 msecs"
  ;("#  # ####  ##  ###  #### #  # ###  ### "
  ; "#  # #    #  # #  #    # # #  #  # #  #"
  ; "#### ###  #    #  #   #  ##   #  # #  #"
  ; "#  # #    #    ###   #   # #  ###  ### "
  ; "#  # #    #  # # #  #    # #  #    # # "
  ; "#  # ####  ##  #  # #### #  # #    #  #")

  )