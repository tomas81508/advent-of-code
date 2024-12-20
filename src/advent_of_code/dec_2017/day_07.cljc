(ns advent-of-code.dec-2017.day-07
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :as string]))

(def test-input ["pbga (66)"
                 "xhth (57)"
                 "ebii (61)"
                 "havc (66)"
                 "ktlj (57)"
                 "fwft (72) -> ktlj, cntj, xhth"
                 "qoyq (66)"
                 "padx (45) -> pbga, havc, qoyq"
                 "tknk (41) -> ugml, padx, fwft"
                 "jptl (61)"
                 "ugml (68) -> gyxo, ebii, jptl"
                 "gyxo (61)"
                 "cntj (57)"])

(def program-pattern #"([^ ]+) \((\d+)\)")

(defn get-values-from-line
  {:test (fn []
           (is= (get-values-from-line "havc (66)")
                {"havc" {:weight 66}})
           (is= (get-values-from-line "fwft (72) -> ktlj, cntj, xhth")
                {"fwft" {:weight   72
                         :children ["ktlj" "cntj" "xhth"]}})

           )}
  [line]
  (let [[program subprograms] (string/split line #" -> ")
        [name weight] (->> (re-find program-pattern program)
                           (drop 1))
        subnames (when subprograms (string/split subprograms #", "))]
    {name (merge {:weight (read-string weight)}
                 (when subnames
                   {:children subnames}))}))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {"qoyq" {:weight 66},
                 "ebii" {:weight 61},
                 "havc" {:weight 66},
                 "ugml" {:weight 68, :children ["gyxo" "ebii" "jptl"]},
                 "cntj" {:weight 57},
                 "jptl" {:weight 61},
                 "xhth" {:weight 57},
                 "pbga" {:weight 66},
                 "gyxo" {:weight 61},
                 "ktlj" {:weight 57},
                 "tknk" {:weight 41, :children ["ugml" "padx" "fwft"]},
                 "padx" {:weight 45, :children ["pbga" "havc" "qoyq"]},
                 "fwft" {:weight 72, :children ["ktlj" "cntj" "xhth"]}}))}
  [input]
  (loop [state {}
         [active-line & rest-lines] input]
    (if-not active-line
      state
      (let [program-part (get-values-from-line active-line)]
        (recur (merge state program-part)
               rest-lines)))))

(def test-state (create-state test-input))

(defn get-root
  {:test (fn []
           (is= (get-root test-state)
                "tknk"))}
  [state]
  (-> (clojure.set/difference (into #{} (keys state))
                              (->> (vals state)
                                   (keep :children)
                                   (flatten)
                                   (into #{})))
      (first)))

(defn get-weight-of-sub-tree
  {:test (fn []
           (is= (get-weight-of-sub-tree test-state "qoyq")
                66)
           (is= (get-weight-of-sub-tree test-state "ugml")
                251))}
  [state name]
  (let [program (get state name)
        subprograms (:children program)]
    (+ (:weight program)
       (if-not subprograms
         0
         (->> subprograms
              (map (fn [name] (get-weight-of-sub-tree state name)))
              (reduce +))))))

(defn add-weights
  [state]
  (reduce (fn [state k]
            (let [total-weight (get-weight-of-sub-tree state k)]
              (assoc-in state [k :total-weight] total-weight)))
          state
          (keys state)))

(def test-state-with-total-weights (add-weights test-state))

(def state (create-state (->> (slurp "src/advent_of_code/dec_2017/day_07_input.txt")
                              (string/split-lines))))

(comment
  ; Solution to part 1
  (get-root state)
  "wiapj"
  )

(def state-with-weights (add-weights state))

(get state-with-weights "wiapj")

(defn get-error-child
  [children]
  (let [error-child-name (->> (reduce (fn [a v]
                                        (update a (:total-weight v) conj (:name v)))
                                      {}
                                      children)
                              (vals)
                              (some (fn [v] (when (= 1 (count v)) v)))
                              (first))]
    (println "ECN" error-child-name)
    (->> children
         (filter (fn [c] (= (:name c) error-child-name)))
         (first))))

(defn get-error-node
  [state root]
  (loop [node (get state root)
         weight-diff nil]
    (let [children (->> (:children node)
                        (map (fn [n] (assoc (get state n) :name n))))
          error-child (get-error-child children)
          weight-diff (if-not error-child
                        weight-diff
                        (- (:total-weight error-child)
                           (->> children
                                (remove error-child)
                                (first)
                                (:total-weight))))]
      (if-not error-child
        {:node node :weight-diff (- (:weight node) weight-diff)}
        (recur error-child weight-diff)))))

(comment
  (:weight-diff (get-error-node state-with-weights "wiapj"))
  1072
  )
