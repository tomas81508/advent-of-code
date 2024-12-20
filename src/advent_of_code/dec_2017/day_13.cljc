(ns advent-of-code.dec-2017.day-13
  (:require [advent-of-code.test :refer [is=]]))

(def input (-> (slurp "src/advent_of_code/dec_2017/day_13_input.txt")
               (clojure.string/split-lines)))

(def test-input ["0: 3"
                 "1: 2"
                 "4: 4"
                 "6: 4"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:scanners [{:depth 3 :position 0 :direction :down}
                            {:depth 2 :position 0 :direction :down}
                            {}
                            {}
                            {:depth 4 :position 0 :direction :down}
                            {}
                            {:depth 4 :position 0 :direction :down}]
                 :position -1
                 :severity 0}))}
  [input]
  {:position -1
   :scanners (->> input
                  (map (fn [l] (->> (re-seq #"\d+" l)
                                    (map read-string))))
                  (reduce (fn [a [index depth]]
                            (-> a
                                (update :result
                                        (fn [scanners]
                                          (concat scanners
                                                  (conj (->> (repeat (- index (inc (:current-index a)))
                                                                     {})
                                                             (into []))
                                                        {:depth depth :position 0 :direction :down}))))
                                (assoc :current-index index)))
                          {:result        []
                           :current-index -1})
                  (:result)
                  (into []))
   :severity 0})

(def test-state (create-state test-input))

(def state (create-state input))

(defn walk-a-step
  [state]
  (let [state (update state :position inc)
        position (:position state)
        actual-scanner (get-in state [:scanners position])]
    (if-not (or (neg? position)
                actual-scanner)
      (:severity state)
      (-> (if (and (not= actual-scanner {})
                   (pos? position)
                   (zero? (:position actual-scanner))
                   (not (zero? (:depth actual-scanner))))
            (update state :severity + (* position (:depth actual-scanner)))
            state)
          (update :scanners (fn [scanners]
                              (->> scanners
                                   (map (fn [s] (cond (= s {})
                                                      s

                                                      (and (= (:direction s) :down)
                                                           (= (inc (:position s)) (:depth s)))
                                                      (-> s
                                                          (update :position dec)
                                                          (assoc :direction :up))

                                                      (= (:direction s) :down)
                                                      (update s :position inc)

                                                      (and (= (:direction s) :up)
                                                           (zero? (:position s)))
                                                      (-> s
                                                          (assoc :position 1)
                                                          (assoc :direction :down))

                                                      :else
                                                      (update s :position dec))))
                                   (into []))))))))

(defn walk
  [state]
  (let [state (walk-a-step state)]
    (if (number? state)
      state
      (recur state))))

(comment
  (walk test-state)
  (walk state)
  )

(defn period
  {:test (fn []
           (is= (period 3) 4)
           (is= (period 2) 2)
           (is= (period 4) 6))}
  [n]
  (* 2 (dec n)))

(defn delay
  [input]
  (->> input
       (map (fn [x] (->> (re-seq #"\d+" x)
                         (map read-string))))
       (reduce (fn [a [index size]]
                 (remove (fn [wait] (zero? (rem (+ index wait) (period size)))) a))
               (range))
       (first)))

(comment
  (delay test-input)
  (time (delay input))
  )





