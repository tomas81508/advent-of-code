(ns advent-of-code.dec-2016.day-11
  (:require [advent-of-code.test :refer [is is-not is=]]
            [advent-of-code.collections :refer [seq-contains?]]
            [clojure.string :refer [split]]
            [clojure.math.combinatorics :refer [combinations]]))


(defn find-part-details
  {:test (fn []
           (is= (find-part-details ["." "." "HG" "."]) ["HG" 3])
           (is= (find-part-details ["E" "." "." "."]) ["E" 1]))}
  [strings]
  (->> (map-indexed (fn [index v] [index v]) strings)
       (remove (fn [[_ v]] (= v ".")))
       (map (fn [[index v]] [v (inc index)]))
       (first)))


(defn create-state
  {:test (fn []
           (is= (create-state "F4 .  .  .  .  .  "
                              "F3 .  .  .  LG .  "
                              "F2 .  HG .  .  .  "
                              "F1 E  .  HM .  LM ")
                {:elevator 1
                 "HG"      2
                 "HM"      1
                 "LG"      3
                 "LM"      1}))}
  [& strings]
  (->> (map (fn [string] (split string #"[ ]+")) strings)
       (apply map list)
       (map reverse)
       (drop 1)
       (reduce (fn [a v]
                 (let [[part level] (find-part-details v)]
                   (cond
                     ; Elevator
                     (= part "E")
                     (assoc a :elevator level)
                     ; Part
                     :else
                     (assoc a part level))))
               {})))

(defn get-parts-by-floor
  {:test (fn []
           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
                                                  "F3 .  .  .  LG .  "
                                                  "F2 .  HG .  .  .  "
                                                  "F1 E  .  HM .  LM ")
                                    1)
                ["HM" "LM"])
           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
                                                  "F3 .  .  .  LG .  "
                                                  "F2 .  HG .  .  .  "
                                                  "F1 E  .  HM .  LM ")
                                    2)
                ["HG"])
           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
                                                  "F3 .  .  .  LG .  "
                                                  "F2 .  .  .  .  .  "
                                                  "F1 E  HG HM .  LM ")
                                    1)
                ["HG" "HM" "LM"]))}
  [state floor]
  (reduce (fn [a k]
            (if (= (get state k) floor)
              (conj a k)
              a))
          []
          (keys (dissoc state :elevator))))


(defn floor-safe?
  {:test (fn []
           ; In other words, if a chip is ever left in the same area as another RTG,
           ; and it's not connected to its own RTG, the chip will be fried.
           (is (floor-safe? ["HG" "HM" "LG"]))
           (is (floor-safe? ["LM"]))
           (is-not (floor-safe? ["HG" "HM" "LM"]))
           (is (floor-safe? [])))}
  [parts]
  (let [microships (filter (fn [part] (= (subs part 1 2) "M")) parts)]
    (reduce (fn [a microship]
              (and a (or (let [material (subs microship 0 1)]
                           (some (fn [part] (= part (str material "G"))) parts))
                         (not (some (fn [part] (re-find #".G" part)) parts)))))
            true
            microships)))


(defn all-floor-safe?
  {:test (fn []
           (is-not (all-floor-safe? (create-state "F4 .  .  .  .  .  "
                                                  "F3 .  .  .  LG .  "
                                                  "F2 .  .  HM .  .  "
                                                  "F1 E  HG .  .  LM ")))
           ; First floor safe since the generator of "H" is connected to its microship.
           (is (all-floor-safe? (create-state "F4 .  .  .  .  .  "
                                              "F3 .  .  .  LG .  "
                                              "F2 .  HG .  .  .  "
                                              "F1 E  .  HM .  LM ")))
           (is (all-floor-safe? (create-state "F4 .  .  .  .  .  "
                                              "F3 .  .  .  .  LM "
                                              "F2 .  .  .  .  .  "
                                              "F1 E  HG HM LG .  "))))}
  [state]
  (reduce (fn [a floor]
            (and a (floor-safe? (get-parts-by-floor state floor))))
          true
          (range 1 5)))

(defn move-parts-up
  {:test (fn []
           (is= (move-parts-up (create-state "F4 .  .  .  .  .  "
                                             "F3 .  .  .  LG .  "
                                             "F2 .  HG .  .  .  "
                                             "F1 E  .  HM .  LM ")
                               ["HM"])
                (create-state "F4 .  .  .  .  .  "
                              "F3 .  .  .  LG .  "
                              "F2 E  HG HM .  .  "
                              "F1 .  .  .  .  LM "))
           (is= (move-parts-up (create-state "F4 .  .  .  .  .  "
                                             "F3 .  .  .  LG .  "
                                             "F2 .  HG .  .  .  "
                                             "F1 E  .  HM .  LM ")
                               ["HM" "LM"])
                (create-state "F4 .  .  .  .  .  "
                              "F3 .  .  .  LG .  "
                              "F2 E  HG HM .  LM  "
                              "F1 .  .  .  .  . ")))}
  [state parts]
  (reduce (fn [state part]
            (update state part inc))
          (update state :elevator inc)
          parts))

(defn move-parts-down
  {:test (fn []
           (is= (move-parts-down (create-state "F4 .  .  .  .  .  "
                                               "F3 .  .  .  LG .  "
                                               "F2 E  HG HM  .  .  "
                                               "F1 .  .  . .  LM ")
                                 ["HG" "HM"])
                (create-state "F4 .  .  .  .  .  "
                              "F3 .  .  .  LG .  "
                              "F2 .  .  .  .  .  "
                              "F1 E  HG HM .  LM ")))}
  [state parts]
  (reduce (fn [state part]
            (update state part dec))
          (update state :elevator dec)
          parts))

(defn get-next-states
  {:test (fn []
           (is= (get-next-states (create-state "F4 .  .  .  .  .  "
                                               "F3 .  .  .  LG .  "
                                               "F2 .  HG .  .  .  "
                                               "F1 E  .  HM .  LM "))
                [(create-state "F4 .  .  .  .  .  "
                               "F3 .  .  .  LG .  "
                               "F2 E  HG HM .  .  "
                               "F1 .  .  .  .  LM ")]))}
  [state]
  (let [single-parts (get-parts-by-floor state (:elevator state))
        double-parts (combinations single-parts 2)
        parts (concat (map (fn [v] [v]) single-parts) double-parts)]
    (->> (reduce (fn [a parts]
                   (cond
                     (= (:elevator state) 1)
                     (conj a (move-parts-up state parts))

                     (= (:elevator state) 4)
                     (conj a (move-parts-down state parts))

                     :else
                     (concat a [(move-parts-up state parts)
                                (move-parts-down state parts)])))
                 []
                 parts)
         (filter all-floor-safe?))))

(defn done?
  {:test (fn []
           (is (done? (create-state "F4 E  HG HM LG LM "
                                    "F3 .  .  .  .  .  "
                                    "F2 .  .  .  .  .  "
                                    "F1 .  .  .  .  .  ")))
           (is-not (done? (create-state "F4 E  .  HM LG LM "
                                        "F3 .  HG .  .  .  "
                                        "F2 .  .  .  .  .  "
                                        "F1 .  .  .  .  .  "))))}
  [state]
  (and (empty? (get-parts-by-floor state 1))
       (empty? (get-parts-by-floor state 2))
       (empty? (get-parts-by-floor state 3))))

(defn construct-finished-state
  {:test (fn []
           (is= (construct-finished-state (create-state "F4 .  .  .  .  .  "
                                                        "F3 .  .  .  LG .  "
                                                        "F2 .  HG .  .  .  "
                                                        "F1 E  .  HM .  LM "))
                (create-state "F4 E  HG HM LG LM "
                              "F3 .  .  .  .  .  "
                              "F2 .  .  .  .  .  "
                              "F1 .  .  .  .  .  ")))}
  [state]
  (reduce (fn [a key]
            (assoc a key 4))
          {}
          (keys state)))

(defn state->simple-state
  {:test (fn []
           (is= (state->simple-state (create-state "F4 .  .  .  .  .  "
                                                   "F3 .  .  .  LG .  "
                                                   "F2 .  HG .  .  .  "
                                                   "F1 E  .  HM .  LM "))
                {:elevator 1
                 :parts    [[2 1] [3 1]]})
           (is= (state->simple-state (create-state "F4 .  .  .  .  .  "
                                                   "F3 .  HG .  .  .  "
                                                   "F2 .  .  .  LG .  "
                                                   "F1 E  .  HM .  LM "))
                {:elevator 1
                 :parts    [[2 1] [3 1]]})
           (is= (state->simple-state (create-state "F4 .  .  .  .  .  "
                                                   "F3 .  .  .  .  .  "
                                                   "F2 .  HG .  LG .  "
                                                   "F1 E  .  HM .  LM "))
                {:elevator 1
                 :parts    [[2 1] [2 1]]}))}
  [state]
  {:elevator (:elevator state)
   :parts    (->> (dissoc state :elevator)
                  (partition 2)
                  (reduce (fn [a [[_ generator-level] [_ microship-level]]]
                            (conj a [generator-level microship-level]))
                          [])
                  (sort))})

(defn compute
  {:test (fn []
           (is= (compute (create-state "F4 .  .  .  "
                                       "F3 .  .  .  "
                                       "F2 .  HG .  "
                                       "F1 E  .  HM "))
                3)
           (is= (compute (create-state "F4 .  .  .  .  .  "
                                       "F3 .  .  .  LG .  "
                                       "F2 .  HG .  .  .  "
                                       "F1 E  .  HM .  LM "))
                11))}
  [state]
  (let [memory-atom (atom {})
        inner-fn (fn inner-fn [state steps]
                   (doseq [next-state (get-next-states state)]
                     (let [memory-steps (get (deref memory-atom) next-state)]
                       (when (or (nil? memory-steps)
                                 (< steps memory-steps))
                         (do
                           (swap! memory-atom assoc next-state steps)
                           (when-not (done? next-state)
                             (inner-fn next-state (inc steps))))))))]
    (inner-fn state 1)
    (get (deref memory-atom) (construct-finished-state state))))

(defn compute2
  {:test (fn []
           (is= (compute2 (create-state "F4 .  .  .  "
                                        "F3 .  .  .  "
                                        "F2 .  HG .  "
                                        "F1 E  .  HM "))
                3)
           (is= (compute2 (create-state "F4 .  .  .  .  .  "
                                        "F3 .  .  .  LG .  "
                                        "F2 .  HG .  .  .  "
                                        "F1 E  .  HM .  LM "))
                11))}
  [state]
  (let [done-state (construct-finished-state state)
        inner-fn (fn [states steps memory]
                   ;(println "On step" steps)
                   ;(println "Number of states" (count states))
                   (let [next-states (reduce (fn [all-next-states state]
                                               (concat all-next-states
                                                       (->> (get-next-states state)
                                                            (filter (fn [s] (nil? (get memory (state->simple-state s))))))))
                                             []
                                             states)]
                     (if (seq-contains? next-states done-state)
                       steps
                       (recur next-states (inc steps) (reduce (fn [a v]
                                                                (assoc a (state->simple-state v) steps))
                                                              memory
                                                              next-states)))))]
    (inner-fn [state] 1 {})))

(defn distinct-by
  {:test (fn []
           (is= (distinct-by identity [{:a 1} {:a 1}])
                [{:a 1}])
           (is= (distinct-by :a [{:a 1 :b 2} {:a 1}])
                [{:a 1 :b 2}])
           (is= (distinct-by :a [{:a 2 :b 2} {:a 1}])
                [{:a 2 :b 2} {:a 1}]))}
  [f coll]
  (let [groups (group-by f coll)]
    (map (fn [x] (first (groups x)))
         (distinct (map f coll)))))

(defn compute3
  {:test (fn []
           (is= (compute3 (create-state "F4 .  .  .  .  .  "
                                        "F3 .  .  .  LG .  "
                                        "F2 .  HG .  .  .  "
                                        "F1 E  .  HM .  LM "))
                11))}
  [state]
  (let [done-simple-state (state->simple-state (construct-finished-state state))
        inner-fn (fn [states steps java-memory-map]
                   ; (println "On step" steps)
                   ; (println "Number of states" (count states))
                   (let [next-states (->> states
                                          (map (fn [state]
                                                 (->> (get-next-states state)
                                                      (filter (fn [s] (nil? (.get java-memory-map (state->simple-state s))))))))
                                          (flatten)
                                          (distinct-by state->simple-state))]
                     (if (.get java-memory-map done-simple-state)
                       steps
                       (do (doseq [next-state next-states]
                             (.put java-memory-map (state->simple-state next-state) steps))
                           (recur next-states (inc steps) java-memory-map)))))]
    (inner-fn [state] 0 (java.util.HashMap.))))

(defn compute4
  {:test (fn []
           (is= (compute4 (create-state "F4 .  .  .  .  .  "
                                        "F3 .  .  .  LG .  "
                                        "F2 .  HG .  .  .  "
                                        "F1 E  .  HM .  LM "))
                11))}
  [state]
  (let [done-simple-state (state->simple-state (construct-finished-state state))
        inner-fn (fn [states steps memory]
                   ; (println "On step" steps)
                   ; (println "Number of states" (count states))
                   (let [next-states (->> states
                                          (map (fn [state]
                                                 (->> (get-next-states state)
                                                      (filter (fn [s] (nil? (get memory (state->simple-state s))))))))
                                          (flatten)
                                          (distinct-by state->simple-state))]
                     ;(println (count (keys memory)))
                     ;(println (count next-states))
                     (if (get memory done-simple-state)
                       steps
                       (recur next-states
                              (inc steps)
                              (reduce (fn [memory next-state]
                                        (assoc memory (state->simple-state next-state) steps))
                                      memory
                                      next-states)))))]
    (inner-fn [state] 0 {})))


(comment puzzle-a
         (is= (time (compute4 (create-state "F4 .  .  .  .  .  .  .  .  .  .  .  "
                                            "F3 .  .  TM .  .  .  .  .  .  .  .  "
                                            "F2 .  TG .  RG RM CG CM .  .  .  .  "
                                            "F1 E  .  .  .  .  .  .  SG SM PG PM ")))
              37))
(comment puzzle-b
         (is= (time (compute3 (create-state "F4 .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  "
                                            "F3 .  .  TM .  .  .  .  .  .  .  .  .  .  .  .  "
                                            "F2 .  TG .  RG RM CG CM .  .  .  .  .  .  .  .  "
                                            "F1 E  .  .  .  .  .  .  SG SM PG PM EG EM DG DM ")))
              61))
(comment "The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
  The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
  The third floor contains a thulium-compatible microchip.
  The fourth floor contains nothing relevant.")
