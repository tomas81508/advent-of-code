(ns advent-of-code.dec-2020.day-23-b
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]
            [advent-of-code.collections :refer [seq-contains?]]))

(def puzzle-input "157623984")

(def test-input "389125467")

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:circle  {3 8
                           8 9
                           9 1
                           1 2
                           2 5
                           5 4
                           4 6
                           6 7
                           7 3}
                 :current 3}))}
  [input]
  {:circle  (loop [[n1 n2 & _ :as numbers] (concat (map (fn [n] (read-string (str n))) input)
                                                   [(read-string (str (first input)))])
                   result {}]
              (if-not n2
                result
                (recur (rest numbers)
                       (assoc result n1 n2))))
   :current (read-string (str (first input)))})

(defn get-destination-cup
  {:test (fn []
           (is= (let [state (create-state test-input)]
                  (get-destination-cup (:circle state) (:current state) [8 9 1]))
                2))}
  [circle current pick-up-cups]
  (let [maybe-destination-cup (if (= current 1)
                                (count circle)
                                (dec current))]
    (if-not (seq-contains? pick-up-cups maybe-destination-cup)
      maybe-destination-cup
      (recur circle maybe-destination-cup pick-up-cups))))

(defn update-current
  {:test (fn []
           (is= (update-current [3 2 8 9 1 5 4 6 7] 3)
                2)
           (is= (update-current [3 2 8 9 1 5 4 6 7] 7)
                3))}
  [cups current]
  (let [current-index (index-of cups current)]
    (nth cups (if (= current-index (dec (count cups))) 0 (inc current-index)))))

(defn move
  {:test (fn []
           (is= (-> (create-state test-input)
                    (move))
                {:circle  {3 2 2 8 8 9 9 1 1 5 5 4 4 6 6 7 7 3}
                 :current 2})
           (is= (-> (create-state test-input)
                    (move)
                    (move))
                {:circle  {3 2 2 5 5 4 4 6 6 7 7 8 8 9 9 1 1 3}
                 :current 5})
           (is= (as-> (create-state test-input) $
                      ((apply comp (repeat 10 move)) $))
                {:circle  {5 8 8 3 3 7 7 4 4 1 1 9 9 2 2 6 6 5}
                 :current 8}))}
  [{circle :circle current :current :as state}]
  (let [cup1 (get circle (:current state))
        cup2 (get circle cup1)
        cup3 (get circle cup2)
        destination-cup (get-destination-cup circle current (list cup1 cup2 cup3))]
    (as-> state $
          (assoc-in $ [:circle (:current state)] (get circle cup3))
          (assoc-in $ [:circle cup3] (get circle destination-cup))
          (assoc-in $ [:circle destination-cup] cup1)
          (assoc $ :current (get-in $ [:circle current])))))

(defn collect-cup-labels
  {:test (fn []
           (is= (collect-cup-labels {5 8 8 3 3 7 7 4 4 1 1 9 9 2 2 6 6 5})
                "92658374"))}
  [cups]
  (loop [current 1
         result ""]
    (let [next (get cups current)]
      (if (= next 1)
        result
        (recur next (str result next))))))

(deftest test-cup-labels-after-100-moves
  (is= (as-> (create-state test-input) $
             ((apply comp (repeat 100 move)) $)
             (:circle $)
             (collect-cup-labels $))
       "67384529"))

(deftest puzzle-a
  (is= (time (as-> (create-state puzzle-input) $
                   ((apply comp (repeat 100 move)) $)
                   (:circle $)
                   (collect-cup-labels $)))
       ; "Elapsed time: 0.419383 msecs"
       "58427369"))

(defn create-state-2
  [input size]
  {:circle  (let [number-count (count input)
                  start-map (loop [[n1 n2 & _ :as numbers] (map (fn [n] (read-string (str n))) input)
                                   result {}]
                              (if-not n2
                                result
                                (recur (rest numbers)
                                       (assoc result n1 n2))))]
              (reduce (fn [a v]
                        (assoc a v (inc v)))
                      (-> start-map
                          (assoc (read-string (str (last input))) (inc number-count))
                          (assoc size (read-string (str (first input)))))
                      (range (inc number-count) size)))
   :current (read-string (str (first input)))})

(defn find-cups-after-1
  [state]
  (let [cup1 (get-in state [:circle 1])
        cup2 (get-in state [:circle cup1])]
    [cup1 cup2]))

(deftest puzzle-test
  (is= (time (as-> (create-state-2 test-input 1000000) $
                   (loop [state $
                          counter 0]
                     (if (= counter 9999999)
                       (move state)
                       (recur (move state) (inc counter))))
                   (find-cups-after-1 $)
                   (apply * $)))
       ; "Elapsed time: 35152.479094 msecs"
       149245887792))

(deftest puzzle-test
  (is= (time (as-> (create-state-2 puzzle-input 1000000) $
                   (loop [state $
                          counter 0]
                     (if (= counter 9999999)
                       (move state)
                       (recur (move state) (inc counter))))
                   (find-cups-after-1 $)
                   (apply * $)))
       ; "Elapsed time: 34562.123593 msecs"
       111057672960))




