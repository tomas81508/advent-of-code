(ns advent-of-code.dec-2015.day-10
  (:require [ysera.test :refer [deftest is=]]))


(def puzzle-input (->> (seq "1113222113")
                       (map (comp read-string str))
                       (into [])))


(defn look-and-say
  {:test (fn []
           (is= (look-and-say [1])
                [1 1])
           (is= (look-and-say [1 1])
                [2 1])
           (is= (look-and-say [2 1])
                [1 2 1 1])
           (is= (look-and-say [1 2 1 1])
                [1 1 1 2 2 1])
           (is= (look-and-say [1 1 1 2 2 1])
                [3 1 2 2 1 1]))}
  [ns]
  (loop [[n & ns] (seq ns)
         previous nil
         stacked 0
         result []]
    (cond (not n)
          (conj result stacked previous)

          (or (not previous) (= n previous))
          (recur ns n (inc stacked) result)

          :else
          (recur ns n 1 (conj result stacked previous)))))

(deftest puzzle-a
         (is= (time (-> ((apply comp (repeat 40 look-and-say)) puzzle-input)
                        (count)))
              ; "Elapsed time: 97.431963 msecs"
              252594))


(deftest puzzle-a
         (is= (time (-> (reduce (fn [ns index]
                              (let [result (time (look-and-say ns))]
                                (println index)
                                result))
                            puzzle-input
                            (range 50))
                        (count)))
              ; "Elapsed time: 901.961603 msecs"
              3579328))
