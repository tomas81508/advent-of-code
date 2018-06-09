;(ns advent-of-code.dec-2016.santa-11b
;  (:require [ysera.test :refer [is= is is-not]]
;            [ysera.collections :refer [seq-contains?]]
;            [clojure.string :refer [split]]
;            [clojure.math.combinatorics]))
;
;
;(defn find-part-details
;  {:test (fn []
;           (is= (find-part-details ["." "." "HG" "."]) ["HG" 3])
;           (is= (find-part-details ["E" "." "." "."]) ["E" 1]))}
;  [strings]
;  (->> (map-indexed (fn [index v] [index v]) strings)
;       (remove (fn [[_ v]] (= v ".")))
;       (map (fn [[index v]] [v (inc index)]))
;       (first)))
;
;
;(defn create-state
;  {:test (fn []
;           (is= (create-state "F4 .  .  .  .  .  "
;                              "F3 .  .  .  LG .  "
;                              "F2 .  HG .  .  .  "
;                              "F1 E  .  HM .  LM ")
;                {:elevator 1
;                 :parts    #{[2 1] [3 1]}}))}
;  [& strings]
;  (let [list-representation (->> (map (fn [string] (split string #"[ ]+")) strings)
;                                 (apply map list)
;                                 (map reverse)
;                                 (drop 1))]
;    {:elevator (second (find-part-details (first list-representation)))
;     :parts    (reduce (fn [parts [generator microship]]
;                         (conj parts [(second (find-part-details generator))
;                                      (second (find-part-details microship))]))
;                       #{}
;                       (->> (drop 1 list-representation)
;                            (partition 2)))}))
;
;;(defn get-parts-by-floor
;;  {:test (fn []
;;           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
;;                                                  "F3 .  .  .  LG .  "
;;                                                  "F2 .  HG .  .  .  "
;;                                                  "F1 E  .  HM .  LM ")
;;                                    1)
;;                ["HM" "LM"])
;;           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
;;                                                  "F3 .  .  .  LG .  "
;;                                                  "F2 .  HG .  .  .  "
;;                                                  "F1 E  .  HM .  LM ")
;;                                    2)
;;                ["HG"])
;;           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
;;                                                  "F3 .  .  .  LG .  "
;;                                                  "F2 .  .  .  .  .  "
;;                                                  "F1 E  HG HM .  LM ")
;;                                    1)
;;                ["HG" "HM" "LM"]))}
;;  [state floor]
;;  (reduce (fn [a k]
;;            (if (= (get state k) floor)
;;              (conj a k)
;;              a))
;;          []
;;          (keys (dissoc state :elevator))))
;
;
;(defn floor-safe?
;  {:test (fn []
;           ; In other words, if a chip is ever left in the same area as another RTG,
;           ; and it's not connected to its own RTG, the chip will be fried.
;           (is (floor-safe? (create-state "F4 .  .  .  .  .  "
;                                          "F3 .  .  .  LG .  "
;                                          "F2 .  .  .  .  .  "
;                                          "F1 E  HG HM .  LM ")
;                            1))
;           (is (floor-safe? (create-state "F4 .  .  .  .  .  "
;                                          "F3 .  .  .  LG .  "
;                                          "F2 .  .  HM .  .  "
;                                          "F1 E  HG .  .  LM ")
;                            2))
;           (is (floor-safe? (create-state "F4 .  .  .  .  .  "
;                                          "F3 .  .  .  LG .  "
;                                          "F2 .  .  HM .  .  "
;                                          "F1 E  HG .  .  LM ")
;                            4))
;           (is-not (floor-safe? (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  LG .  "
;                                              "F2 .  .  HM .  .  "
;                                              "F1 E  HG .  .  LM ")
;                                1)))}
;  [state floor]
;  (let [unbalanced-parts (->> (:parts state)
;                              (filter (fn [[g-l m-l]] (and (or (= g-l floor)
;                                                               (= m-l floor))
;                                                           (not= g-l m-l)))))
;        contains-generator? (fn [coll]
;                              (not (empty? (filter (fn [[g-l _]] (= g-l floor)) coll))))
;        contains-microship? (fn [coll]
;                              (not (empty? (filter (fn [[_ m-l]] (= m-l floor)) coll))))]
;    (not (and (contains-generator? unbalanced-parts)
;              (contains-microship? unbalanced-parts)))))
;
;
;(defn all-floor-safe?
;  {:test (fn []
;           (is-not (all-floor-safe? (create-state "F4 .  .  .  .  .  "
;                                                  "F3 .  .  .  LG .  "
;                                                  "F2 .  .  HM .  .  "
;                                                  "F1 E  HG .  .  LM ")))
;           ; First floor safe since the generator of "H" is connected to its microship.
;           (is (all-floor-safe? (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  LG .  "
;                                              "F2 .  HG .  .  .  "
;                                              "F1 E  .  HM .  LM ")))
;           (is (all-floor-safe? (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  .  LM "
;                                              "F2 .  .  .  .  .  "
;                                              "F1 E  HG HM LG .  "))))}
;  [state]
;  (reduce (fn [a floor]
;            (and a (floor-safe? state floor)))
;          true
;          (range 1 5)))
;
;(defn move-parts-up
;  {:test (fn []
;           (is= (move-parts-up (create-state "F4 .  .  .  .  .  "
;                                             "F3 .  .  .  LG .  "
;                                             "F2 .  HG .  .  .  "
;                                             "F1 E  .  HM .  LM ")
;                               ["HM"])
;                (create-state "F4 .  .  .  .  .  "
;                              "F3 .  .  .  LG .  "
;                              "F2 E  HG HM .  .  "
;                              "F1 .  .  .  .  LM "))
;           (is= (move-parts-up (create-state "F4 .  .  .  .  .  "
;                                             "F3 .  .  .  LG .  "
;                                             "F2 .  HG .  .  .  "
;                                             "F1 E  .  HM .  LM ")
;                               ["HM" "LM"])
;                (create-state "F4 .  .  .  .  .  "
;                              "F3 .  .  .  LG .  "
;                              "F2 E  HG HM .  LM  "
;                              "F1 .  .  .  .  . ")))}
;  [state parts]
;  (reduce (fn [state part]
;            (update state part inc))
;          (update state :elevator inc)
;          parts))
;
;(defn move-parts-down
;  {:test (fn []
;           (is= (move-parts-down (create-state "F4 .  .  .  .  .  "
;                                               "F3 .  .  .  LG .  "
;                                               "F2 E  HG HM  .  .  "
;                                               "F1 .  .  . .  LM ")
;                                 ["HG" "HM"])
;                (create-state "F4 .  .  .  .  .  "
;                              "F3 .  .  .  LG .  "
;                              "F2 .  .  .  .  .  "
;                              "F1 E  HG HM .  LM ")))}
;  [state parts]
;  (reduce (fn [state part]
;            (update state part dec))
;          (update state :elevator dec)
;          parts))
;
;(defn get-parts-by-floor
;  {:test (fn []
;           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  LG .  "
;                                              "F2 .  HG .  .  .  "
;                                              "F1 E  .  HM .  LM ")
;                                1)
;               [[3 1] [2 1]])
;           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  LG .  "
;                                              "F2 .  HG .  .  .  "
;                                              "F1 E  .  HM .  LM ")
;                                3)
;                [[3 1]])
;           (is= (get-parts-by-floor (create-state "F4 .  .  .  .  .  "
;                                                  "F3 .  .  .  LG .  "
;                                                  "F2 .  HG .  .  .  "
;                                                  "F1 E  .  HM .  LM ")
;                                    4)
;                []))}
;  [state floor]
;  (->> (:parts state)
;       (filter (fn [[g-l m-l]] (or (= g-l floor)
;                                   (= m-l floor))))))
;
;(defn contains-parts?
;  {:test (fn []
;           (is (contains-parts? (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  LG .  "
;                                              "F2 .  HG .  .  .  "
;                                              "F1 E  .  HM .  LM ")
;                                1))
;           (is (contains-parts? (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  LG .  "
;                                              "F2 .  HG .  .  .  "
;                                              "F1 E  .  HM .  LM ")
;                                3))
;           (is-not (contains-parts? (create-state "F4 .  .  .  .  .  "
;                                              "F3 .  .  .  LG .  "
;                                              "F2 .  HG .  .  .  "
;                                              "F1 E  .  HM .  LM ")
;                                4)))}
;  [state floor]
;  (not (empty? (get-parts-by-floor state floor))))
;
;(defn get-next-states
;  {:test (fn []
;           (is= (get-next-states (create-state "F4 .  .  .  .  .  "
;                                               "F3 .  .  .  LG .  "
;                                               "F2 .  HG .  .  .  "
;                                               "F1 E  .  HM .  LM "))
;                [(create-state "F4 .  .  .  .  .  "
;                               "F3 .  .  .  LG .  "
;                               "F2 E  HG HM .  .  "
;                               "F1 .  .  .  .  LM ")]))}
;  [state]
;  (let [state {:elevator 1, :parts #{[3 1] [2 1]}}
;        single-parts (get-parts-by-floor state (:elevator state))
;        double-parts (clojure.math.combinatorics/combinations single-parts 2)
;        parts (concat (map (fn [v] [v]) single-parts) double-parts)]
;    (println parts)
;    (->> (reduce (fn [a parts]
;                   (cond
;                     (= (:elevator state) 1)
;                     (conj a (move-parts-up state parts))
;
;                     (= (:elevator state) 4)
;                     (conj a (move-parts-down state parts))
;
;                     :else
;                     (concat a [(move-parts-up state parts)
;                                (move-parts-down state parts)])))
;                 []
;                 parts)
;         (filter all-floor-safe?))))
;
;(defn done?
;  {:test (fn []
;           (is (done? (create-state "F4 E  HG HM LG LM "
;                                    "F3 .  .  .  .  .  "
;                                    "F2 .  .  .  .  .  "
;                                    "F1 .  .  .  .  .  ")))
;           (is-not (done? (create-state "F4 E  .  HM LG LM "
;                                        "F3 .  HG .  .  .  "
;                                        "F2 .  .  .  .  .  "
;                                        "F1 .  .  .  .  .  "))))}
;  [state]
;  (and (not (contains-parts? state 1))
;       (not (contains-parts? state 2))
;       (not (contains-parts? state 3))))
;
;
;(defn construct-finished-state
;  {:test (fn []
;           (is= (construct-finished-state (create-state "F4 .  .  .  .  .  "
;                                                        "F3 .  .  .  LG .  "
;                                                        "F2 .  HG .  .  .  "
;                                                        "F1 E  .  HM .  LM "))
;                (create-state "F4 E  HG HM LG LM "
;                              "F3 .  .  .  .  .  "
;                              "F2 .  .  .  .  .  "
;                              "F1 .  .  .  .  .  ")))}
;  [state]
;  (reduce (fn [a key]
;            (assoc a key 4))
;          {}
;          (keys state)))
;
;(defn state->simple-state
;  {:test (fn []
;           (is= (state->simple-state (create-state "F4 .  .  .  .  .  "
;                                                   "F3 .  .  .  LG .  "
;                                                   "F2 .  HG .  .  .  "
;                                                   "F1 E  .  HM .  LM "))
;                {:elevator 1
;                 :parts    #{[2 1] [3 1]}})
;           (is= (state->simple-state (create-state "F4 .  .  .  .  .  "
;                                                   "F3 .  HG .  .  .  "
;                                                   "F2 .  .  .  LG .  "
;                                                   "F1 E  .  HM .  LM "))
;                {:elevator 1
;                 :parts    #{[2 1] [3 1]}}))}
;  [state]
;  {:elevator (:elevator state)
;   :parts    (->> (dissoc state :elevator)
;                  (partition 2)
;                  (reduce (fn [a [[_ generator-level] [_ microship-level]]]
;                            (conj a [generator-level microship-level]))
;                          #{}))})
;;
;;(defn compute
;;  {:test (fn []
;;           (is= (compute (create-state "F4 .  .  .  "
;;                                       "F3 .  .  .  "
;;                                       "F2 .  HG .  "
;;                                       "F1 E  .  HM "))
;;                3)
;;           (is= (compute (create-state "F4 .  .  .  .  .  "
;;                                       "F3 .  .  .  LG .  "
;;                                       "F2 .  HG .  .  .  "
;;                                       "F1 E  .  HM .  LM "))
;;                11))}
;;  [state]
;;  (let [memory-atom (atom {})
;;        inner-fn (fn inner-fn [state steps]
;;                   (doseq [next-state (get-next-states state)]
;;                     (let [memory-steps (get (deref memory-atom) next-state)]
;;                       (when (or (nil? memory-steps)
;;                                 (< steps memory-steps))
;;                         (do
;;                           (swap! memory-atom assoc next-state steps)
;;                           (when-not (done? next-state)
;;                             (inner-fn next-state (inc steps))))))))]
;;    (inner-fn state 1)
;;    (get (deref memory-atom) (construct-finished-state state))))
;;
;;(defn compute2
;;  {:test (fn []
;;           (is= (compute2 (create-state "F4 .  .  .  "
;;                                        "F3 .  .  .  "
;;                                        "F2 .  HG .  "
;;                                        "F1 E  .  HM "))
;;                3)
;;           (is= (compute2 (create-state "F4 .  .  .  .  .  "
;;                                        "F3 .  .  .  LG .  "
;;                                        "F2 .  HG .  .  .  "
;;                                        "F1 E  .  HM .  LM "))
;;                11))}
;;  [state]
;;  (let [done-state (construct-finished-state state)
;;        inner-fn (fn [states steps memory]
;;                   ;(println "On step" steps)
;;                   ;(println "Number of states" (count states))
;;                   (let [next-states (reduce (fn [all-next-states state]
;;                                               (concat all-next-states
;;                                                       (->> (get-next-states state)
;;                                                            (filter (fn [s] (nil? (get memory (state->simple-state s))))))))
;;                                             []
;;                                             states)]
;;                     (if (seq-contains? next-states done-state)
;;                       steps
;;                       (recur next-states (inc steps) (reduce (fn [a v]
;;                                                                (assoc a (state->simple-state v) steps))
;;                                                              memory
;;                                                              next-states)))))]
;;    (inner-fn [state] 1 {})))
;;
;;(defn compute3
;;  {:test (fn []
;;           ;(is= (compute3 (create-state "F4 .  .  .  "
;;           ;                             "F3 .  .  .  "
;;           ;                             "F2 .  HG .  "
;;           ;                             "F1 E  .  HM "))
;;           ;     3)
;;           (is= (compute3 (create-state "F4 .  .  .  .  .  "
;;                                        "F3 .  .  .  LG .  "
;;                                        "F2 .  HG .  .  .  "
;;                                        "F1 E  .  HM .  LM "))
;;                11))}
;;  [state]
;;  (let [done-simple-state (state->simple-state (construct-finished-state state))
;;        java-memory-map (java.util.HashMap.)
;;        inner-fn (fn [states steps]
;;                   (println "On step" steps)
;;                   (println "Number of states" (count states))
;;                   (let [next-states (time (->> states
;;                                                (map (fn [state]
;;                                                       (->> (get-next-states state)
;;                                                            (filter (fn [s] (nil? (.get java-memory-map (state->simple-state s))))))))
;;                                                (flatten)))]
;;                     (if (time (or (.get java-memory-map done-simple-state) (empty? next-states)))
;;                       steps
;;                       (do (time (doseq [next-state next-states]
;;                                   (.put java-memory-map (state->simple-state next-state) steps)))
;;                           (recur next-states (inc steps))))))]
;;    (inner-fn [state] 0)))
;;
;(defn compute4
;  {:test (fn []
;           ;(is= (compute4 (create-state "F4 .  .  .  "
;           ;                             "F3 .  .  .  "
;           ;                             "F2 .  HG .  "
;           ;                             "F1 E  .  HM "))
;           ;     3)
;           (is= (compute4 (create-state "F4 .  .  .  .  .  "
;                                        "F3 .  .  .  LG .  "
;                                        "F2 .  HG .  .  .  "
;                                        "F1 E  .  HM .  LM "))
;                11))}
;  [state]
;  (let [done-simple-state (state->simple-state (construct-finished-state state))
;        inner-fn (fn [states steps memory]
;                   (println "On step" steps)
;                   (println "Number of states" (count states))
;                   (let [next-states (time (->> states
;                                                (map (fn [state]
;                                                       (->> (get-next-states state)
;                                                            (filter (fn [s] (nil? (get memory (state->simple-state s))))))))
;                                                (flatten)))]
;                     (if (time (or (get memory done-simple-state) (empty? next-states)))
;                       steps
;                       (recur next-states
;                              (inc steps)
;                              (time (reduce (fn [memory next-state]
;                                              (assoc memory (state->simple-state next-state) steps))
;                                            memory
;                                            next-states))))))]
;    (inner-fn [state] 0 {})))
;
;
;(comment (compute4 (create-state "F4 .  .  .  .  .  .  .  .  .  .  .  "
;                                 "F3 .  .  TM .  .  .  .  .  .  .  .  "
;                                 "F2 .  TG .  RG RM CG CM .  .  .  .  "
;                                 "F1 E  .  .  .  .  .  .  SG SM PG PM ")))
;(comment "The first floor contains a strontium generator, a strontium-compatible microchip, a plutonium generator, and a plutonium-compatible microchip.
;  The second floor contains a thulium generator, a ruthenium generator, a ruthenium-compatible microchip, a curium generator, and a curium-compatible microchip.
;  The third floor contains a thulium-compatible microchip.
;  The fourth floor contains nothing relevant.")
