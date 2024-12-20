(ns advent-of-code.dec-2022.day-11
  (:require [advent-of-code.test :refer [is=]]))

(def input (slurp "src/advent_of_code/dec_2022/day_11_input.txt"))
(def test-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1")

(def pattern #"Monkey \d+:\n  Starting items: ([^\n]*)\n  Operation: new = ([^ ]+) (.) ([^ ]+)\n  Test: divisible by (\d+)\n    If true: throw to monkey (\d+)\n    If false: throw to monkey (\d+)")

(defn monkey->data
  {:test (fn []
           (is= (monkey->data "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3")
                {:items     [79 98]
                 :operation ["old" "*" "19"]
                 :inspects  0
                 :test      23
                 :true      2
                 :false     3}))}
  [monkey-input]
  (let [[_ items x-1 operation x-2 test t f] (re-find pattern monkey-input)]
    {:items     (->> (clojure.string/split items #", ")
                     (map read-string))
     :operation [x-1 operation x-2]
     :inspects  0
     :test      (read-string test)
     :true      (read-string t)
     :false     (read-string f)}))

(defn transform-data
  [s]
  (as-> s $
        (clojure.string/split $ #"\n\n")
        (map monkey->data $)
        (into [] $)))

(def test-state (transform-data test-input))
(def input-state (transform-data input))

(defn operate
  {:test (fn []
           (is= (operate ["old" "*" "19"] 2)
                38))}
  [[x-1 op x-2] v]
  ((condp = op "*" *' "+" +')
   (if (= x-1 "old") v (read-string x-1))
   (if (= x-2 "old") v (read-string x-2))))

(defn common-divider
  [state]
  (->> state
       (map :test)
       (apply *)))

(def part-2-divider (common-divider input-state))

(defn monkey-turn
  [part state n]
  (reduce (fn [state worry]
            (let [monkey (get state n)
                  new-worry (as-> (operate (:operation monkey) worry) $
                                  (if (= part \a)
                                    (quot $ 3)
                                    (mod $ part-2-divider)))
                  new-monkey (if (zero? (mod new-worry (:test monkey))) (:true monkey) (:false monkey))]
              (-> state
                  (update-in [n :inspects] inc)
                  (update-in [new-monkey :items] (fn [items] (concat items [new-worry]))))))
          (assoc-in state [n :items] [])
          (get-in state [n :items])))

(defn monkey-round
  [part state]
  (reduce (partial monkey-turn part)
          state
          (range (count state))))


(comment
  ; part a
  (time (->> input-state
             ((apply comp (repeat 20 (partial monkey-round \a))))
             (map :inspects)
             (sort >)
             (take 2)
             (apply *)))
  ; "Elapsed time: 16.196852 msecs"
  ; => 64032
  )


(comment
  ; part b
  (time (let [monkey-fn (partial monkey-round \b)]
          (->> (reduce (fn [state _]
                         (monkey-fn state))
                       input-state
                       (range 10000))
               (map :inspects)
               (sort >)
               (take 2)
               (apply *))))
  ; "Elapsed time: 1316.730549 msecs"
  ; => 12729522272
  )
