(ns advent-of-code.dec-2021.day-08
  (:require [advent-of-code.test :refer [is=]]))

(def test-input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
(def larger-example "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")
(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_08_input.txt"))

(defn create-row
  [input-row]
  (let [[_ signal-input output-signal] (re-matches #"(.*) \| (.*)" input-row)]
    {:signal-pattern (->> (clojure.string/split signal-input #" ")
                          (mapv (fn [chars] (map (fn [c] (keyword (str c))) (seq chars)))))
     :output-value   output-signal}))

(defn create-state
  [input]
  (->> input
       (clojure.string/split-lines)
       (map create-row)))

(def test-state (create-row test-input))

(def larger-example-state (create-state larger-example))

(def puzzle-state (create-state puzzle-input))

(defn solver-a
  {:test (fn []
           (is= (solver-a larger-example-state) 26))}
  [state]
  (->> state
       (map :output-value)
       (map (fn [output-value]
              (->> (clojure.string/split output-value #" ")
                   (map count))))
       (flatten)
       (reduce (fn [a v]
                 (condp = v
                   2 (update a 1 inc)
                   3 (update a 7 inc)
                   4 (update a 4 inc)
                   7 (update a 8 inc)
                   a))
               {1 0 4 0 7 0 8 0})
       (vals)
       (reduce +)))

(comment
  (time (solver-a puzzle-state))
  ; "Elapsed time: 1.431771 msecs"
  )



(def segments {0 #{:a :b :c :e :f :g}
               1 #{:c :f}
               2 #{:a :c :d :e :g}
               3 #{:a :c :d :f :g}
               4 #{:b :c :d :f}
               5 #{:a :b :d :f :g}
               6 #{:a :b :d :e :f :g}
               7 #{:a :c :f}
               8 #{:a :b :c :d :e :f :g}
               9 #{:a :b :c :d :f :g}})

(def segments-invert (clojure.set/map-invert segments))


(defn get-a
  {:test (fn []
           (is= (get-a [[:a :c :e :d :g :f :b]
                        [:c :d :f :b :e]
                        [:g :c :d :f :a]
                        [:f :b :c :a :d]
                        [:d :a :b]
                        [:c :e :f :a :b :d]
                        [:c :d :f :g :e :b]
                        [:e :a :f :b]
                        [:c :a :g :e :d :b]
                        [:a :b]])
                :d))}
  [signal-pattern]
  (let [one (some (fn [n] (when (= (count n) 2) n)) signal-pattern)
        seven (some (fn [n] (when (= (count n) 3) n)) signal-pattern)]
    (first (remove (set one) seven))))


(defn get-d
  {:test (fn []
           (is= (get-d [[:a :c :e :d :g :f :b]
                        [:c :d :f :b :e]
                        [:g :c :d :f :a]
                        [:f :b :c :a :d]
                        [:d :a :b]
                        [:c :e :f :a :b :d]
                        [:c :d :f :g :e :b]
                        [:e :a :f :b]
                        [:c :a :g :e :d :b]
                        [:a :b]]
                       #{:c :f})
                :f))}
  [signal-pattern possibilities]
  (let [four (some (fn [n] (when (= (count n) 4) n)) signal-pattern)]
    (first (clojure.set/intersection (set four) possibilities))))

(defn get-translator-using-frequencies
  {:test (fn []
           (is= (get-translator-using-frequencies [[:e :g :c :b :d :f :a]
                                                   [:e :c :b :d :f]
                                                   [:g :c :d :f :a]
                                                   [:c :b :d :f :a]
                                                   [:b :d :a]
                                                   [:e :c :b :d :f :a]
                                                   [:e :g :c :b :d :f]
                                                   [:e :b :f :a]
                                                   [:e :g :c :b :d :a]
                                                   [:b :a]])
                {:a #{:a :d}
                 :b :e
                 :c #{:a :d}
                 :d #{:c :f}
                 :e :g
                 :f :b
                 :g #{:c :f}}))}
  [signal-pattern]
  (->> signal-pattern
       (map seq)
       (reduce concat)
       (frequencies)
       (reduce-kv (fn [a k v]
                    (condp = v
                      4 (assoc a :e k)
                      6 (assoc a :b k)
                      7 (-> a
                            (update :d conj k)
                            (update :g conj k))
                      8 (-> a
                            (update :a conj k)
                            (update :c conj k))
                      9 (assoc a :f k)
                      a))
                  {:a #{}
                   :c #{}
                   :d #{}
                   :g #{}})))

(defn get-translator
  {:test (fn []
           (is= (get-translator [#{:e :g :c :b :d :f :a}
                                 #{:e :c :b :d :f}
                                 #{:g :c :d :f :a}
                                 #{:c :b :d :f :a}
                                 #{:b :d :a}
                                 #{:e :c :b :d :f :a}
                                 #{:e :g :c :b :d :f}
                                 #{:e :b :f :a}
                                 #{:e :g :c :b :d :a}
                                 #{:b :a}])
                {:a :d
                 :c :a
                 :d :f
                 :g :c
                 :b :e
                 :e :g
                 :f :b}))}
  [signal-pattern]
  (let [translator (get-translator-using-frequencies signal-pattern)
        a (get-a signal-pattern)
        d (get-d signal-pattern (:d translator))
        c (first (disj (:c translator) a))
        g (first (disj (:g translator) d))]
    (assoc translator :a a :d d :c c :g g)))

(defn calculate-row-sum
  {:test (fn []
           (is= (-> (create-row test-input)
                    (calculate-row-sum))
                5353))}
  [{signal-pattern :signal-pattern output-value :output-value}]
  (let [translator (get-translator signal-pattern)
        translator-inverse (clojure.set/map-invert translator)]
    (->> (clojure.string/split output-value #" ")
         (map (fn [ns]
                (->> ns
                     (map (fn [n] (translator-inverse (keyword (str n)))))
                     (set))))
         (map segments-invert)
         (drop-while zero?)
         (map str)
         (clojure.string/join)
         (read-string))))

(defn solver-b
  {:test (fn []
           (is= (solver-b larger-example-state)
                61229))}
  [state]
  (->> state
       (map calculate-row-sum)
       (reduce +)))

(comment
  (time (solver-b puzzle-state))
  ; "Elapsed time: 14.402859 msecs"
  )



