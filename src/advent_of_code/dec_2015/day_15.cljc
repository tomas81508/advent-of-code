(ns advent-of-code.dec-2015.day-15
  (:require [advent-of-code.test :refer [is=]]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.edn :as edn]
            [clojure.walk :refer [postwalk]]))

(def input ["Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5"
            "Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8"
            "Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6"
            "Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1"])

(def test-input ["Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8"
                 "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"])

(def line-pattern #"(\w+): capacity ([-\d]+), durability ([-\d]+), flavor ([-\d]+), texture ([-\d]+), calories ([-\d]+)")

(defn parse-line
  {:test (fn []
           (is= (parse-line "Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5")
                ["Frosting" 4 -2 0 0 5]))}
  [text]
  (let [[_ n capacity durability flavor texture calories] (re-find line-pattern text)]
    [n
     (edn/read-string capacity)
     (edn/read-string durability)
     (edn/read-string flavor)
     (edn/read-string texture)
     (edn/read-string calories)]))

(defn create-ingredients
  [input]
  (->> input
       (map parse-line)
       (reduce (fn [a [n capacity durability flavor texture calories]]
                 (assoc a n {:capacity   capacity
                             :durability durability
                             :flavor     flavor
                             :texture    texture
                             :calories   calories}))
               {})))

(def ingredients (create-ingredients input))
(def test-ingredients (create-ingredients test-input))


(defn make-cookie
  {:test (fn []
           (is= (make-cookie test-ingredients {"Butterscotch" 44 "Cinnamon" 56})
                62842880))}
  [ingredients amounts]
  (->> (reduce-kv (fn [a k v]
                    (let [ingredient (get ingredients k)]
                      (conj a (reduce-kv (fn [a k ov] (assoc a k (* v ov)))
                                         ingredient
                                         ingredient))))
                  []
                  amounts)
       (reduce (fn [a v]
                 (-> a
                     (update :capacity + (:capacity v))
                     (update :durability + (:durability v))
                     (update :flavor + (:flavor v))
                     (update :texture + (:texture v))))
               {:capacity 0 :durability 0 :flavor 0 :texture 0})
       (vals)
       (map (fn [x] (max x 0)))
       (apply *)))

(defn find-best-cookie
  [ingredients]
  (->>
    (let [ingredients-seq (seq ingredients)]
      (->> (for [a (range 101)
                 b (range 101)
                 c (range 101)
                 d (range 101)
                 :when (= (+ a b c d) 100)]
             [a b c d])
           (map (fn [distribution]
                  (make-cookie ingredients (->> distribution
                                                (map-indexed (fn [index item] [index item]))
                                                (reduce (fn [a [index item]]
                                                          (assoc a (first (nth ingredients-seq index)) item))
                                                        {})))))))
    (apply max)))

(comment
  (find-best-cookie ingredients)
  )

(defn make-cookie-2
  [ingredients amounts]
  (let [cookie (->> (reduce-kv (fn [a k v]
                                 (let [ingredient (get ingredients k)]
                                   (conj a (reduce-kv (fn [a k ov] (assoc a k (* v ov)))
                                                      ingredient
                                                      ingredient))))
                               []
                               amounts)
                    (reduce (fn [a v]
                              (-> a
                                  (update :capacity + (:capacity v))
                                  (update :durability + (:durability v))
                                  (update :flavor + (:flavor v))
                                  (update :texture + (:texture v))
                                  (update :calories + (:calories v))))
                            {:capacity 0 :durability 0 :flavor 0 :texture 0 :calories 0}))]
    (if (= (:calories cookie) 500)
      (->> (dissoc cookie :calories)
           (vals)
           (map (fn [x] (max x 0)))
           (apply *))
      0)))

(defn find-best-cookie-2
  [ingredients]
  (->>
    (let [ingredients-seq (seq ingredients)]
      (->> (for [a (range 101)
                 b (range 101)
                 c (range 101)
                 d (range 101)
                 :when (= (+ a b c d) 100)]
             [a b c d])
           (map (fn [distribution]
                  (make-cookie-2 ingredients (->> distribution
                                                  (map-indexed (fn [index item] [index item]))
                                                  (reduce (fn [a [index item]]
                                                            (assoc a (first (nth ingredients-seq index)) item))
                                                          {})))))))
    (apply max)))

(comment
  (find-best-cookie-2 ingredients)
  )
