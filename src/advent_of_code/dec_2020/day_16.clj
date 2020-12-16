(ns advent-of-code.dec-2020.day-16
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (slurp "src/advent_of_code/dec_2020/day_16.txt"))

(def test-input "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:fields         {"class" [[1 3] [5 7]]
                                  "row"   [[6 11] [33 44]]
                                  "seat"  [[13 40] [45 50]]}
                 :your-ticket    [7 1 14]
                 :nearby-tickets [[7 3 47]
                                  [40 4 50]
                                  [55 2 20]
                                  [38 6 12]]}))}
  [input]
  (let [[fields your-ticket nearby-tickets] (clojure.string/split input #"\n\n")]
    {:fields         (as-> (clojure.string/split-lines fields) $
                           (map (fn [line]
                                  (let [pattern #"([^:]+): ([\d]+)-([\d]+) or ([\d]+)-([\d]+)"
                                        [_ field n1 n2 n3 n4] (re-matches pattern line)]
                                    {field [[(read-string n1) (read-string n2)]
                                            [(read-string n3) (read-string n4)]]}))
                                $)
                           (apply merge $))
     :your-ticket    (let [data (subs your-ticket 13)]
                       (->> (clojure.string/split data #",")
                            (map read-string)
                            (vec)))
     :nearby-tickets (let [data (subs nearby-tickets 16)]
                       (as-> data $
                             (clojure.string/split-lines $)
                             (map (fn [line]
                                    (->> (clojure.string/split line #",")
                                         (map read-string)))
                                  $)))}))

(defn valid-field?
  [state value field]
  (let [[[n1 n2] [n3 n4]] (get-in state [:fields field])]
    (or (<= n1 value n2)
        (<= n3 value n4))))

(defn valid-number?
  {:test (fn []
           (is (valid-number? (create-state test-input) 47))
           (is-not (valid-number? (create-state test-input) 4)))}
  [state value]
  (->> (:fields state)
       (keys)
       (filter (fn [k] (valid-field? state value k)))
       (first)))

(defn invalid-values
  {:test (fn []
           (is= (invalid-values (create-state test-input))
                [4 55 12]))}
  [state]
  (let [all-values (flatten (:nearby-tickets state))]
    (remove (partial valid-number? state) all-values)))

(deftest puzzle-a
         (is= (->> (get-puzzle-input)
                   (create-state)
                   (invalid-values)
                   (apply +))
              28882))

(def test-input-2 "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9")

(defn discard-invalid-tickets
  {:test (fn []
           (is= (discard-invalid-tickets (create-state test-input))
                {:fields         {"class" [[1 3] [5 7]]
                                  "row"   [[6 11] [33 44]]
                                  "seat"  [[13 40] [45 50]]}
                 :your-ticket    [7 1 14]
                 :nearby-tickets [[7 3 47]]}))}
  [state]
  (update state :nearby-tickets
          (fn [tickets]
            (remove (fn [ticket]
                      (some (fn [n] (not (valid-number? state n))) ticket))
                    tickets))))

(defn find-fields
  {:test (fn []
           (is= (-> (create-state test-input-2)
                    (discard-invalid-tickets)
                    (find-fields))
                ["row" "class" "seat"]))}
  [state]
  (let [fields-values (as-> (conj (:nearby-tickets state) (:your-ticket state)) $
                            (apply map (fn [& field-values] field-values) $))
        possible-fields-coll (->> fields-values
                                  (map (fn [field-values]
                                         (->> (:fields state)
                                              (keys)
                                              (filter (fn [k]
                                                        (every? (fn [field-value]
                                                                  (valid-field? state field-value k))
                                                                field-values)))))))]
    (loop [fields-coll possible-fields-coll]
      (let [alone-fields (->> fields-coll
                              (filter (fn [fields]
                                        (and (coll? fields)
                                             (= (count fields) 1))))
                              (map first)
                              (set))]
        (if (empty? alone-fields)
          (flatten fields-coll)
          (recur (map (fn [fields]
                        (if-not (coll? fields)
                          fields
                          (if (= (count fields) 1)
                            (first fields)
                            (remove (fn [field] (contains? alone-fields field))
                                    fields))))
                      fields-coll)))))))

(deftest puzzle-b
         (is= (time (let [state (-> (create-state (get-puzzle-input))
                                    (discard-invalid-tickets))
                          fields-order (find-fields state)]
                      (->> fields-order
                           (map-indexed (fn [index field] [index field]))
                           (filter (fn [[_ field]] (clojure.string/starts-with? field "departure")))
                           (map (fn [[index _]] index))
                           (map (fn [index] (get-in state [:your-ticket index])))
                           (apply *))))
              ; "Elapsed time: 71.722925 msecs"
              1429779530273))







