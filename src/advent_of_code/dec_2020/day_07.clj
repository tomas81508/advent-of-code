(ns advent-of-code.dec-2020.day-07
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2020/day_07.txt")
       (clojure.string/split-lines)))

(def test-input
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
   "bright white bags contain 1 shiny gold bag."
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
   "faded blue bags contain no other bags."
   "dotted black bags contain no other bags."])

(defn bag->keyword
  {:test (fn []
           (is= (bag->keyword "light red bags") :light-red-bag)
           (is= (bag->keyword "bright white bag") :bright-white-bag))}
  [bag-as-string]
  (-> (if (clojure.string/ends-with? bag-as-string "s")
        (subs bag-as-string 0 (dec (count bag-as-string)))
        bag-as-string)
      (clojure.string/replace #" " "-")
      (keyword)))

(defn parse-text
  {:test (fn []
           (is= (parse-text "light red bags contain 1 bright white bag, 2 muted yellow bags.")
                {:light-red-bag {:bright-white-bag 1 :muted-yellow-bag 2}})
           (is= (parse-text "dotted black bags contain no other bags.")
                {:dotted-black-bag {}}))}
  [text]
  (let [pattern (re-pattern "([\\w| ]+) contain ([\\w|\\d| |,]+).")
        [_ key-part values-part] (re-matches pattern text)
        value-parts (clojure.string/split values-part #", ")]
    {(bag->keyword key-part)
     (reduce (fn [a v]
               (let [pattern (re-pattern "([\\d]+) ([\\w| ]+)")
                     [_ n bag-as-string] (re-matches pattern v)]
                 (if-not n
                   a
                   (assoc a (bag->keyword bag-as-string) (read-string n)))))
             {}
             value-parts)}))

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:light-red-bag    {:bright-white-bag 1 :muted-yellow-bag 2}
                 :muted-yellow-bag {:shiny-gold-bag 2 :faded-blue-bag 9}
                 :dark-orange-bag  {:bright-white-bag 3 :muted-yellow-bag 4}
                 :vibrant-plum-bag {:faded-blue-bag 5 :dotted-black-bag 6}
                 :dotted-black-bag {}
                 :bright-white-bag {:shiny-gold-bag 1}
                 :faded-blue-bag   {}
                 :shiny-gold-bag   {:dark-olive-bag 1 :vibrant-plum-bag 2}
                 :dark-olive-bag   {:faded-blue-bag 3 :dotted-black-bag 4}}))}
  [input]
  (reduce (fn [state v]
            (merge state (parse-text v)))
          {}
          input))

(defn get-containing-bags
  {:test (fn []
           (let [state (create-state test-input)]
             (is= (get-containing-bags state :light-red-bag) #{})
             (is= (get-containing-bags state :shiny-gold-bag)
                  #{:muted-yellow-bag :bright-white-bag :light-red-bag :dark-orange-bag})))}
  [state bag]
  (loop [result #{bag}]
    (let [containers (->> result
                          (map (fn [b]
                                 (reduce-kv (fn [a k v]
                                              (if (contains? v b)
                                                (conj a k)
                                                a))
                                            #{}
                                            state))))
          new-result (apply clojure.set/union result containers)]
      (if (= new-result result)
        (disj result bag)
        (recur new-result)))))

(deftest puzzle-a
  (is= (-> (get-puzzle-input)
           (create-state)
           (get-containing-bags :shiny-gold-bag)
           (count))
       164))

(def test-input-2
  ["shiny gold bags contain 2 dark red bags."
   "dark red bags contain 2 dark orange bags."
   "dark orange bags contain 2 dark yellow bags."
   "dark yellow bags contain 2 dark green bags."
   "dark green bags contain 2 dark blue bags."
   "dark blue bags contain 2 dark violet bags."
   "dark violet bags contain no other bags."])

(defn inner-bags
  {:test (fn []
           (is= (inner-bags (create-state test-input) :shiny-gold-bag)
                {:dark-olive-bag   1
                 :vibrant-plum-bag 2
                 :faded-blue-bag   13
                 :dotted-black-bag 16})
           (is= (inner-bags (create-state test-input-2) :shiny-gold-bag)
                {:dark-red-bag    2
                 :dark-orange-bag 4
                 :dark-yellow-bag 8
                 :dark-green-bag  16
                 :dark-blue-bag   32
                 :dark-violet-bag 64}))}
  [state bag]
  (loop [bags {bag 1}
         result {}]
    (let [{bags :bags result :result}
          (reduce-kv (fn [a b n]
                       (let [inner-bags (get state b)]
                         (-> (if (= inner-bags {})
                               a
                               (update a :bags (fn [bags]
                                                 (->> inner-bags
                                                      (reduce-kv (fn [a k v]
                                                                   (if (contains? a k)
                                                                     (update a k (fn [x] (+ x (* n v))))
                                                                     (assoc a k (* n v))))
                                                                 bags)))))
                             (update :result (fn [result]
                                               (update result b (fn [x] (+ (or x 0) n))))))))
                     {:bags {} :result result}
                     bags)]
      (if (= bags {})
        (dissoc result bag)
        (recur bags result)))))

(defn count-inner-bags
  {:test (fn []
           (is= (count-inner-bags (create-state test-input) :shiny-gold-bag)
                32)
           (is= (count-inner-bags (create-state test-input-2) :shiny-gold-bag)
                126))}
  [state bag]
  (apply + (vals (inner-bags state bag))))

(deftest puzzle-b
  (is= (as-> (get-puzzle-input) $
             (create-state $)
             (inner-bags $ :shiny-gold-bag)
             (vals $)
             (apply + $))
       7872))


