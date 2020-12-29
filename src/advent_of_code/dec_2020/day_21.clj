(ns advent-of-code.dec-2020.day-21
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def puzzle-input (->> (slurp "src/advent_of_code/dec_2020/day_21.txt")
                       (clojure.string/split-lines)))

(def test-input ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
                 "trh fvjkl sbzzf mxmxvkd (contains dairy)"
                 "sqjhc fvjkl (contains soy)"
                 "sqjhc mxmxvkd sbzzf (contains fish)"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                [{:ingredients ["mxmxvkd" "kfcds" "sqjhc" "nhms"]
                  :allergens   ["dairy" "fish"]}
                 {:ingredients ["trh" "fvjkl" "sbzzf" "mxmxvkd"]
                  :allergens   ["dairy"]}
                 {:ingredients ["sqjhc" "fvjkl"]
                  :allergens   ["soy"]}
                 {:ingredients ["sqjhc" "mxmxvkd" "sbzzf"]
                  :allergens   ["fish"]}]))}
  [input]
  (->> input
       (map (fn [food]
              (let [[_ ingredients-string allergens-string]
                    (re-matches #"([\w| ]+) \(contains ([\w| |,]+)\)" food)]
                {:ingredients (clojure.string/split ingredients-string #" ")
                 :allergens   (clojure.string/split allergens-string #", ")})))))

(def test-state (create-state test-input))
(def state (create-state puzzle-input))

(defn naive-allergen->food
  {:test (fn []
           (is= (naive-allergen->food test-state)
                {"dairy" #{"mxmxvkd"}
                 "fish"  #{"sqjhc" "mxmxvkd"}
                 "soy"   #{"sqjhc" "fvjkl"}}))}
  [state]
  (->> state
       (reduce (fn [a {allergens   :allergens
                       ingredients :ingredients}]
                 (reduce (fn [a allergen]
                           (update a allergen conj (set ingredients)))
                         a
                         allergens))
               {})
       (reduce-kv (fn [a k v]
                    (assoc a k (apply clojure.set/intersection v)))
                  {})))

(defn any-match?
  {:test (fn []
           (is= (any-match? {"nuts"  "abc"
                             "dairy" #{"mxmxvkd"}
                             "fish"  #{"sqjhc" "mxmxvkd"}
                             "soy"   #{"sqjhc" "fvjkl"}})
                "mxmxvkd"))}
  [allergen->food-state]
  (->> (vals allergen->food-state)
       (some (fn [ingredients]
               (and (set? ingredients)
                    (= (count ingredients) 1)
                    (first ingredients))))))

(defn remove-ingredient
  {:test (fn []
           (is= (remove-ingredient {"dairy" #{"mxmxvkd"}
                                    "fish"  #{"sqjhc" "mxmxvkd"}
                                    "soy"   #{"sqjhc" "fvjkl"}}
                                   "mxmxvkd")
                {"dairy" "mxmxvkd"
                 "fish"  #{"sqjhc"}
                 "soy"   #{"sqjhc" "fvjkl"}}))}
  [state ingredient]
  (reduce-kv (fn [state k v]
               (cond (= v #{ingredient})
                     (assoc state k ingredient)

                     (set? v)
                     (update state k clojure.set/difference #{ingredient})

                     :else
                     state))
             state
             state))

(defn get-allergen->food
  {:test (fn []
           (is= (get-allergen->food test-state)
                {"dairy" "mxmxvkd"
                 "fish"  "sqjhc"
                 "soy"   "fvjkl"}))}
  [state]
  (loop [state (naive-allergen->food state)]
    (if-let [ingredient (any-match? state)]
      (recur (remove-ingredient state ingredient))
      state)))

(defn all-ingredients
  {:test (fn []
           (is= (all-ingredients test-state)
                #{"sqjhc" "fvjkl" "nhms" "trh" "kfcds" "sbzzf" "mxmxvkd"}))}
  [state]
  (->> state
       (map :ingredients)
       (apply concat)
       (set)))

(defn free-from-allergen-ingredients
  {:test (fn []
           (is= (free-from-allergen-ingredients test-state)
                #{"nhms" "trh" "kfcds" "sbzzf"}))}
  [state]
  (clojure.set/difference (all-ingredients state)
                          (set (vals (get-allergen->food state)))))

(defn count-allergen-free-ingredients
  {:test (fn []
           (is= (count-allergen-free-ingredients test-state)
                5))}
  [state]
  (let [allergen-free-ingredients (free-from-allergen-ingredients state)]
    (->> state
         (map :ingredients)
         (map (fn [ingredients]
                (reduce (fn [a ingredient]
                          (if (contains? allergen-free-ingredients ingredient)
                            (inc a)
                            a))
                        0
                        ingredients)))
         (apply +))))

(deftest puzzle-a
         (is= (count-allergen-free-ingredients state)
              1945))

(deftest puzzle-b
         (is= (->> (get-allergen->food state)
                   (seq)
                   (sort-by first)
                   (map second)
                   (clojure.string/join ","))
              "pgnpx,srmsh,ksdgk,dskjpq,nvbrx,khqsk,zbkbgp,xzb"))


