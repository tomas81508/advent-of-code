(ns advent-of-code.dec-2017.day-25
  (:require [advent-of-code.test :refer [is=]]))

(def state {:tape         [0]
            :current      0
            :lowest-index 0})

(defn get-current-index
  [state]
  (- (:current state) (:lowest-index state)))

(defn move-right
  {:test (fn []
           (is= (move-right state)
                {:tape         [0 0]
                 :current      1
                 :lowest-index 0}))}
  [state]
  (-> (if (= (dec (count (:tape state)))
             (get-current-index state))
        (update state :tape conj 0)
        state)
      (update :current inc)))

(defn move-left
  {:test (fn []
           (is= (move-left state)
                {:tape         [0 0]
                 :current      -1
                 :lowest-index -1}))}
  [state]
  (-> (if (< (dec (:current state)) (:lowest-index state))
        (-> state
            (update :tape (fn [tape] (into [] (cons 0 tape))))
            (update :lowest-index dec))
        state)
      (update :current dec)))

(defn write-value
  [state value]
  (let [current-index (get-current-index state)]
    (assoc-in state [:tape current-index] value)))

(defn run-stage
  [state stage n]
  (let [tape (:tape state)
        current (:current state)
        lowest-index (:lowest-index state)
        current-index (- current lowest-index)
        current-value (get tape current-index)]
    (if (zero? n)
      (reduce + (:tape state))
      (condp = stage
        "A" (if (zero? current-value)
              (-> state
                  (write-value 1)
                  (move-right)
                  (recur "B" (dec n)))
              (-> state
                  (write-value 0)
                  (move-right)
                  (recur "C" (dec n))))
        "B" (if (zero? current-value)
              (-> state
                  (write-value 0)
                  (move-left)
                  (recur "A" (dec n)))
              (-> state
                  (write-value 0)
                  (move-right)
                  (recur "D" (dec n))))
        "C" (if (zero? current-value)
              (-> state
                  (write-value 1)
                  (move-right)
                  (recur "D" (dec n)))
              (-> state
                  (write-value 1)
                  (move-right)
                  (recur "A" (dec n))))
        "D" (if (zero? current-value)
              (-> state
                  (write-value 1)
                  (move-left)
                  (recur "E" (dec n)))
              (-> state
                  (write-value 0)
                  (move-left)
                  (recur "D" (dec n))))
        "E" (if (zero? current-value)
              (-> state
                  (write-value 1)
                  (move-right)
                  (recur "F" (dec n)))
              (-> state
                  (write-value 1)
                  (move-left)
                  (recur "B" (dec n))))
        "F" (if (zero? current-value)
              (-> state
                  (write-value 1)
                  (move-right)
                  (recur "A" (dec n)))
              (-> state
                  (write-value 1)
                  (move-right)
                  (recur "E" (dec n))))
        ))))

(comment
  (run-stage state "A" 12399302)
  2794
  )

