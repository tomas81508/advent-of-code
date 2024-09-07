(ns advent-of-code.dec-2023.day-25
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.test :refer [deftest]]
            [ysera.collections :refer [seq-contains?]]))

(def input (->> (slurp "src/advent_of_code/dec_2023/day_25_input.txt")
                (clojure.string/split-lines)
                (into [])))

(def test-input ["jqt: rhn xhk nvd"
                 "rsh: frs pzl lsr"
                 "xhk: hfx"
                 "cmg: qnr nvd lhk bvb"
                 "rhn: xhk bvb hfx"
                 "bvb: xhk hfx"
                 "pzl: lsr hfx nvd"
                 "qnr: nvd"
                 "ntq: jqt hfx bvb xhk"
                 "nvd: lhk"
                 "lsr: lhk"
                 "rzs: qnr cmg lsr rsh"
                 "frs: qnr lhk lsr"])

(defn create-state
  [input]
  (reduce (fn [a line]
            (let [[_ node nodes] (re-find #"(\w+): (.+)" line)
                  other-nodes (clojure.string/split nodes #" ")]
              (-> (reduce (fn [a other-node]
                            (update a other-node (fn [xs] (conj (or xs #{}) node))))
                          a
                          other-nodes)
                  (update node (fn [xs] (reduce conj (or xs #{}) other-nodes))))))
          {}
          input))

(def test-state (create-state test-input))
(def state (create-state input))

(comment
  (create-state test-input)
  )

(defn get-connected-nodes
  [state node]
  (loop [result #{}
         boundary #{node}]
    (if (empty? boundary)
      result
      (recur (reduce conj result boundary)
             (clojure.set/difference
               (reduce (fn [boundary node]
                         (reduce conj boundary (get state node)))
                       #{}
                       boundary)
               result)
             ))))

(defn walk-a-step
  [state path]
  (let [current-node (first path)]
    (->> (get state current-node)
         (remove (fn [n] (seq-contains? path n)))
         (map (fn [n] (cons n path))))))

(defn find-distance-path
  [state node1 node2]
  (if (= node1 node2)
    (list node1)
    (loop [paths [(list node1)]]
      (let [new-paths (reduce (fn [a path]
                                (let [new-paths (walk-a-step state path)]
                                  (reduce conj a new-paths)))
                              (list)
                              paths)
            distance-path (->> new-paths
                               (filter (fn [[n & _]] (= n node2)))
                               (first))]
        (or distance-path
            (recur new-paths))))))

(defn random-traffic
  [state n]
  (->> (range n)
       (map (fn [i] (let [node1 (rand-nth (keys state))
                          node2 (rand-nth (keys state))]
                      (println "Distance " i)
                      (->> (find-distance-path state node1 node2)
                           (partition 2)
                           (frequencies)))))
       (remove empty?)
       (reduce (fn [a m]
                 ;(println a m)
                 (reduce-kv (fn [a k v]
                              (update a k (fn [ov] (+ (or ov 0) v))))
                            a
                            m))
               {})))

(comment
  (get-connected-nodes test-state "jqt")
  (find-distance-path test-state "hfx" "cmg")
  ; hfx/pzl, bvb/cmg, and nvd/jqt
  (random-traffic test-state 1000)
  (random-traffic state 1000)

  )

; [["qfj" "tbq"] 57]
; [["tbq" "qfj"] 47]
; [["dsr" "xzn"] 38]
; [["xzn" "dsr"] 36]
; [["xbl" "qqh"] 33]
; [["qqh" "xbl"] 30]
; [["tbq" "nxb"] 24]
; [["xlp" "qqh"] 21]

(defn remove-edges
  [state edges]
  (reduce (fn [state [n1 n2]]
            (-> state
                (update n1 disj n2)
                (update n2 disj n1)))
          state
          edges))

(defn solve
  [state edges]
  (let [number-of-nodes (count (keys state))
        trios (combinations edges 3)]
    (loop [[es & trios] trios]
      (let [state (remove-edges state es)
            connected-nodes (get-connected-nodes state "clm")]
        (if (= (count connected-nodes) number-of-nodes)
          (if (empty? trios)
            :error
            (recur trios))
          es)))))

(comment
  (solve state [["qfj" "tbq"] ["tbq" "qfj"] ["dsr" "xzn"]
                ["xzn" "dsr"] ["xbl" "qqh"] ["qqh" "xbl"]])
  ; => (["qfj" "tbq"] ["dsr" "xzn"] ["xbl" "qqh"])
  )

(def separated-state (remove-edges state [["qfj" "tbq"] ["dsr" "xzn"] ["xbl" "qqh"]]))

(def group-1 (get-connected-nodes separated-state "qfj"))
(def group-2 (get-connected-nodes separated-state "tbq"))

(* (count group-1) (count group-2))






