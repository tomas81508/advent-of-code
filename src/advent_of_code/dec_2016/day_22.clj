(ns advent-of-code.dec-2016.day-22
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]))

(defn create-initial-nodes
  [input]
  (reduce (fn [a row]
            (let [[_ x y size used] (re-find #"\/dev\/grid\/node-x(\d+)-y(\d+)[ ]+(\d+)T[ ]+(\d+)T" row)]
              (assoc a
                [(read-string x) (read-string y)]
                {:size (read-string size)
                 :used (read-string used)})))
          {}
          input))

(def nodes (->> (slurp "src/advent_of_code/dec_2016/day_22_input.txt")
                (string/split-lines)
                (drop 2)
                (create-initial-nodes)))

(defn find-viable-pair
  [nodes node]
  (->> nodes
       (seq)
       (keep (fn [[_ n]]
               (when (and (not= node n)
                          (not (zero? (:used node)))
                          (<= (:used node) (- (:size n) (:used n))))
                 n)))))

(defn count-all-viable-pairs
  {:test (fn []
           (is= (count-all-viable-pairs nodes)
                981))}
  [nodes]
  (->> nodes
       (reduce-kv (fn [a _ node]
                    (+ a (count (find-viable-pair nodes node))))
                  0)))


(def test-nodes (->> ["Filesystem            Size  Used  Avail  Use%"
                      "/dev/grid/node-x0-y0   10T    8T     2T   80%"
                      "/dev/grid/node-x0-y1   11T    6T     5T   54%"
                      "/dev/grid/node-x0-y2   32T   28T     4T   87%"
                      "/dev/grid/node-x1-y0    9T    7T     2T   77%"
                      "/dev/grid/node-x1-y1    8T    0T     8T    0%"
                      "/dev/grid/node-x1-y2   11T    7T     4T   63%"
                      "/dev/grid/node-x2-y0   10T    6T     4T   60%"
                      "/dev/grid/node-x2-y1    9T    8T     1T   88%"
                      "/dev/grid/node-x2-y2    9T    6T     3T   66%"]
                     (drop 1)
                     (create-initial-nodes)))

(defn enough-capacity?
  [nodes node capacity]
  (>= (get-in nodes [node :size]) capacity))

(defn move-in-direction
  [state d]
  (let [empty-node-position (:empty-node-position state)
        nodes (:nodes state)
        new-empty-node-position (map + empty-node-position d)]
    (when (and (contains? nodes new-empty-node-position)
               (enough-capacity? nodes empty-node-position (get-in nodes [new-empty-node-position :used])))
      (-> state
          (assoc-in [:nodes empty-node-position :used] (get-in nodes [new-empty-node-position :used]))
          (assoc-in [:nodes new-empty-node-position :used] 0)
          (assoc :empty-node-position new-empty-node-position)
          ((fn [state]
             (if (= new-empty-node-position (:goal-position state))
               (assoc state :goal-position empty-node-position)
               state)))))))

(def directions [[-1 0] [0 -1] [1 0] [0 1]])

(defn get-new-states
  {:test (fn []
           (is= (-> (get-new-states {:nodes               test-nodes
                                     :empty-node-position [1 1]
                                     :goal-position       [2 0]})
                    (nth 1)
                    (get-new-states)
                    (nth 1)
                    (get-new-states)
                    (nth 1)
                    (get-new-states)
                    (nth 0)
                    (get-new-states)
                    (nth 0)
                    (get-new-states)
                    (nth 0)
                    (get-new-states)
                    (nth 0)
                    (:goal-position))
                [0 0])
           )}
  [state]
  (->> directions
       (keep (fn [d]
               (move-in-direction state d)))))

(defn get-min-steps-to-goal-node
  {:test (fn []
           (is= (get-min-steps-to-goal-node test-nodes)
                7))}
  [nodes]
  (let [nodes nodes]
    (loop [states [{:nodes               nodes
                    :goal-position       [(->> nodes
                                               (seq)
                                               (map ffirst)
                                               (apply max)) 0]
                    :empty-node-position (->> nodes
                                              (seq)
                                              (some (fn [[p n]] (when (zero? (:used n)) p))))}]
           steps 0]
      (if (->> states
               (some (fn [{goal :goal-position}] (when (= goal [0 0]) true))))
        steps
        (recur (->> states
                    (reduce (fn [a state]
                              (apply conj a (get-new-states state)))
                            []))
               (inc steps)))))
  )

(comment
  (->> (for [y (range 29)
             x (range 35)]
         [x y])
       (partition 35)
       (map (fn [row]
              (reduce (fn [a p]
                        (str a
                             (let [node (get nodes p)]
                               (cond (zero? (:used node)) "0"
                                     (> (:size node) 100) "L"
                                     (= p [34 0]) "G"
                                     (= p [0 0]) "R"
                                     :else "."))))
                      ""
                      row))))
  )

"Look at the map above"
"It takes 68 steps to go to the right corner"
"It takes 5*33=165 steps to move G to R"
(+ 68 165)

