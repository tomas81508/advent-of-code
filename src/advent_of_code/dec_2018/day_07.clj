(ns advent-of-code.dec-2018.day-07
  (:require [advent-of-code.collections :refer [seq-contains?]]
            [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]
            [clojure.string :as string]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2018/day_07.txt")
       (string/split-lines)))

(def test-data ["Step C must be finished before step A can begin."
                "Step C must be finished before step F can begin."
                "Step A must be finished before step B can begin."
                "Step A must be finished before step D can begin."
                "Step B must be finished before step E can begin."
                "Step D must be finished before step E can begin."
                "Step F must be finished before step E can begin."])

(def test-prerequisites {"A" ["C"]
                         "B" ["A"]
                         "D" ["A"]
                         "E" ["B" "D" "F"]
                         "F" ["C"]})

(defn string-data->map-data
  {:test (fn []
           (is= (string-data->map-data "Step C must be finished before step A can begin.")
                {:prerequisite "C"
                 :task         "A"}))}
  [string-data]
  {:post [(not (nil? (:task %))) (not (nil? (:prerequisite %)))]}
  (let [pattern (re-pattern "Step (\\w) must be finished before step (\\w) can begin.")]
    (let [[_ p t] (re-find pattern string-data)]
      {:prerequisite p :task t})))


(defn create-state-part-1
  {:test (fn []
           (is= (create-state-part-1 test-data)
                {:done-tasks    []
                 :undone-tasks  ["A" "B" "C" "D" "E" "F"]
                 :prerequisites test-prerequisites}))}
  [data]
  {:done-tasks    []
   :undone-tasks  (->> data
                       (map string-data->map-data)
                       (reduce (fn [a {p :prerequisite t :task}]
                                 (conj a p t))
                               #{})
                       (into [])
                       (sort))
   :prerequisites (->> data
                       (map string-data->map-data)
                       (reduce (fn [a {p :prerequisite t :task}]
                                 (if (contains? a t)
                                   (update a t conj p)
                                   (assoc a t [p])))
                               {}))})


(defn do-one-task
  {:test (fn []
           (is= (-> (create-state-part-1 test-data)
                    (do-one-task))
                {:done-tasks    ["C"]
                 :undone-tasks  ["A" "B" "D" "E" "F"]
                 :prerequisites {"B" ["A"]
                                 "D" ["A"]
                                 "E" ["B" "D" "F"]}}))}
  [state]
  (loop [[undone-task & rest-of-undone-tasks] (:undone-tasks state)]
    (if (contains? (:prerequisites state) undone-task)
      (recur rest-of-undone-tasks)
      (-> state
          (update :done-tasks conj undone-task)
          (update :undone-tasks (fn [undone-tasks]
                                  (remove (fn [u-t]
                                            (= u-t undone-task))
                                          undone-tasks)))
          (update :prerequisites (fn [prerequisites]
                                   (reduce-kv (fn [a k v]
                                                (if-not (seq-contains? v undone-task)
                                                  a
                                                  (if (= (count (get a k)) 1)
                                                    (dissoc a k)
                                                    (update a k (fn [values]
                                                                  (remove (fn [v]
                                                                            (= v undone-task))
                                                                          values))))))
                                              prerequisites
                                              prerequisites)))))))

(defn do-all-tasks
  {:test (fn []
           (is= (-> (create-state-part-1 test-data)
                    (do-all-tasks))
                {:done-tasks    ["C" "A" "B" "D" "F" "E"]
                 :undone-tasks  []
                 :prerequisites {}}))}
  [state]
  (let [state (do-one-task state)]
    (if (empty? (:undone-tasks state))
      state
      (recur state))))

(deftest puzzle-part-1
  (is= (->> (get-puzzle-input)
            (create-state-part-1)
            (do-all-tasks)
            (:done-tasks)
            (string/join ""))
       "IJLFUVDACEHGRZPNKQWSBTMXOY"))


;;;; PART 2


(defn create-state-part-2
  {:test (fn []
           (is= (create-state-part-2 test-data 2 0)
                {:time          -1
                 :workers       {0 :free 1 :free}
                 :delay         0
                 :done-tasks    []
                 :undone-tasks  ["A" "B" "C" "D" "E" "F"]
                 :prerequisites test-prerequisites}))}
  [data number-of-workers delay]
  (merge (create-state-part-1 data)
         {:time    -1
          :workers (reduce (fn [a v] (assoc a v :free))
                           {}
                           (range number-of-workers))
          :delay   delay}))


(defn get-task-time
  {:test (fn []
           (is= (get-task-time "A" 0) 1)
           (is= (get-task-time "Z" 0) 26)
           (is= (get-task-time "A" 60) 61)
           (is= (get-task-time "Z" 60) 86))}
  [task delay]
  {:pre [(re-matches (re-pattern "[A-Z]") task)]}
  (+ delay (- (int (first task)) 64)))


(defn worker-free?
  {:test (fn []
           (is (-> (create-state-part-2 test-data 2 0)
                   (worker-free? 0))))}
  [state worker-id]
  (= (get-in state [:workers worker-id]) :free))


(defn add-task-to-worker
  {:test (fn []
           (is= (-> (create-state-part-2 test-data 2 0)
                    (add-task-to-worker "C" 1)
                    (select-keys [:workers :undone-tasks]))
                {:workers      {0 :free 1 {:task "C" :time-left (get-task-time "C" 0)}}
                 :undone-tasks ["A" "B" "C" "D" "E" "F"]}))}
  [state task worker-id]
  (assoc-in state [:workers worker-id] {:task task :time-left (get-task-time task (:delay state))}))


(defn get-time-left
  {:test (fn []
           (is= (-> (create-state-part-2 test-data 2 0)
                    (add-task-to-worker "C" 1)
                    (get-time-left 1))
                3))}
  [state worker-id]
  (get-in state [:workers worker-id :time-left]))


(defn work-with-tasks
  {:test (fn []
           (is= (-> (create-state-part-2 test-data 3 0)
                    (add-task-to-worker "A" 0)
                    (add-task-to-worker "E" 1)
                    (work-with-tasks)
                    (select-keys [:workers :done-tasks]))
                {:workers    {0 :free
                              1 {:task      "E"
                                 :time-left 4}
                              2 :free}
                 :done-tasks ["A"]}))}
  [state]
  (reduce (fn [state worker-id]
            (cond (worker-free? state worker-id)
                  state

                  (= (get-time-left state worker-id) 1)
                  (let [task (get-in state [:workers worker-id :task])]
                    (-> state
                        (assoc-in [:workers worker-id] :free)
                        (update :undone-tasks (fn [tasks] (remove (fn [x] (= x task)) tasks)))
                        (update :done-tasks conj task)))

                  :else
                  (update-in state [:workers worker-id :time-left] dec)))
          state
          (keys (:workers state))))


(defn task-in-progress?
  {:test (fn []
           (is (-> (create-state-part-2 test-data 2 0)
                   (assoc-in [:workers 1] {:task "C"})
                   (task-in-progress? "C")))
           (is-not (-> (create-state-part-2 test-data 2 0)
                       (assoc-in [:workers 1] {:task "C"})
                       (task-in-progress? "A"))))}
  [state task]
  (some (fn [worker]
          (= (:task worker) task))
        (vals (:workers state))))


(defn get-available-tasks
  {:test (fn []
           (is= (-> (create-state-part-2 test-data 2 0)
                    (get-available-tasks))
                ["C"])
           (is= (-> {:undone-tasks  ["A" "B" "D" "E" "F"]
                     :done-tasks    ["C"]
                     :prerequisites test-prerequisites}
                    (get-available-tasks))
                ["A" "F"]))}
  [state]
  (->> (:undone-tasks state)
       (remove (fn [task] (task-in-progress? state task)))
       (filter (fn [task]
                 (let [prs (get (:prerequisites state) task)]
                   (or (nil? prs)
                       (reduce (fn [a pr]
                                 (and a (seq-contains? (:done-tasks state) pr)))
                               true
                               prs)))))))


(defn assign-new-tasks
  {:test (fn []
           (is= (-> (create-state-part-2 test-data 2 0)
                    (assign-new-tasks)
                    (select-keys [:workers]))
                {:workers {0 {:task      "C"
                              :time-left 3}
                           1 :free}}))}
  [state]
  (reduce (fn [state worker-id]
            (let [available-task (-> (get-available-tasks state)
                                     (first))]
              (if (and available-task (worker-free? state worker-id))
                (add-task-to-worker state available-task worker-id)
                state)))
          state
          (keys (:workers state))))


(defn time-tick
  {:test (fn []
           (is= (-> (create-state-part-2 test-data 2 0)
                    (time-tick)
                    (time-tick)
                    (time-tick)
                    (time-tick)
                    (time-tick)
                    (select-keys [:workers :time]))
                {:time    4
                 :workers {0 {:task      "B"
                              :time-left 2}
                           1 {:task      "F"
                              :time-left 5}}}))}
  [state]
  (-> state
      (work-with-tasks)
      (assign-new-tasks)
      (update :time inc)))


(defn do-all-tasks-simultaneously
  {:test (fn []
           (is= (-> (create-state-part-2 test-data 2 0)
                    (do-all-tasks-simultaneously)
                    (select-keys [:done-tasks :time]))
                {:done-tasks ["C" "A" "B" "F" "D" "E"]
                 :time       15}))}
  [state]
  (let [state (time-tick state)]
    (if (empty? (:undone-tasks state))
      state
      (recur state))))

(deftest puzzle-part-2
  (is= (-> (get-puzzle-input)
           (create-state-part-2 5 60)
           (do-all-tasks-simultaneously)
           (select-keys [:done-tasks :time]))
       {:done-tasks ["I" "J" "L" "V" "F" "D" "U" "H" "A" "C" "E" "R" "G" "Z" "P" "N" "Q" "K" "W" "S" "B" "T" "M" "X" "O" "Y"]
        :time       1072}))
