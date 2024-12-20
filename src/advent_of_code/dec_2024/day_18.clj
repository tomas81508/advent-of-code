(ns advent-of-code.dec-2024.day-18
  (:require [advent-of-code.test :refer [is=]]))

(def input (slurp "src/advent_of_code/dec_2024/day_18_input.txt"))
(def test-input "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0")

(defn get-obstacles
  {:test (fn []
           (is= (get-obstacles test-input 3)
                #{[5 4] [4 2] [4 5]}))}
  [input n]
  (->> (clojure.string/split-lines input)
       (take n)
       (reduce (fn [a line]
                 (let [[x y] (->> (re-seq #"\d+" line)
                                  (map read-string))]
                   (conj a [x y])))
               #{})))

(defn create-state
  [input n max-value]
  {:end-position [max-value max-value]
   :max-value    max-value
   :visited      #{[0 0]}
   :boundary     #{[0 0]}
   :steps        0
   :obstacles    (get-obstacles input n)})

(def test-state (create-state test-input 12 6))
(def directions [[1 0] [-1 0] [0 -1] [0 1]])

(defn get-next-positions
  {:test (fn []
           (is= (->> (get-next-positions test-state [0 0])
                     (into #{}))
                #{[1 0] [0 1]})
           (is= (->> (get-next-positions (update test-state :visited conj [0 0]) [1 0])
                     (into #{}))
                #{[2 0] [1 1]}))}
  [state position]
  (->> directions
       (map (fn [d] (mapv + position d)))
       (remove (fn [np] (or (contains? (:visited state) np)
                            (contains? (:obstacles state) np)
                            (not (<= 0 (first np) (:max-value state)))
                            (not (<= 0 (second np) (:max-value state))))))))

(defn walk-a-step
  {:test (fn []
           (let [new-state (walk-a-step test-state)]
             (is= (:boundary new-state)
                  #{[1 0] [0 1]})
             (is= (:visited new-state)
                  #{[1 0] [0 1] [0 0]})
             (is= (:steps new-state) 1)))}
  [state]
  (let [new-boundary (reduce (fn [a p]
                               (reduce conj a (get-next-positions state p)))
                             #{}
                             (:boundary state))]
    (-> state
        (assoc :boundary new-boundary)
        (update :steps inc)
        (update :visited clojure.set/union new-boundary))))

(defn walk
  {:test (fn []
           (is= (walk test-state) 22))}
  [state]
  (let [end-position (:end-position state)]
    (loop [state state]
      (cond
        (contains? (:boundary state) end-position)
        (:steps state)

        (empty? (:boundary state))
        :fail

        :else
        (recur (walk-a-step state))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input 12 6) "6,1"))}
  [input start-n max-value]
  (let [lines (clojure.string/split-lines input)
        max-n (count (clojure.string/split-lines input))]
    (loop [max-n max-n
           min-n start-n]
      (println max-n min-n)
      (if (<= max-n (inc min-n))
        (nth lines (dec max-n))
        (let [n (int (/ (+ max-n min-n) 2))
              state (create-state input n max-value)
              result (walk state)]
          (if (= result :fail)
            (recur n min-n)
            (recur max-n n)))))))

(def state (create-state input 1024 70))

(comment
  (time (walk state))
  ;; "Elapsed time: 25.268792 msecs"
  ;=> 296

  (time (part-2 input 1024 70))
  ;; "Elapsed time: 131.295709 msecs"
  ;=> "28,44"
  )
