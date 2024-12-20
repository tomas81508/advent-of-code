(ns advent-of-code.dec-2020.day-19-part-a
  (:require [advent-of-code.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]))

(defn get-puzzle-input-rules []
  (-> (slurp "src/advent_of_code/dec_2020/day_19_rules.txt")
      (clojure.string/split-lines)))

(defn get-puzzle-input-messages []
  (-> (slurp "src/advent_of_code/dec_2020/day_19_messages.txt")
      (clojure.string/split-lines)))

(def test-rules ["0: 4 1 5"
                 "1: 2 3 | 3 2"
                 "2: 4 4 | 5 5"
                 "3: 4 5 | 5 4"
                 "4: \"a\""
                 "5: \"b\""
                 "6: 5"])

(defn create-rules
  {:test (fn []
           (is= (create-rules test-rules)
                {0 [[4 1 5]]
                 1 [[2 3] [3 2]]
                 2 [[4 4] [5 5]]
                 3 [[4 5] [5 4]]
                 4 "a"
                 5 "b"
                 6 [[5]]}))}
  [input]
  (reduce (fn [a v]
            (let [[_ n rules-as-string] (re-matches #"([\d]+): (.*)" v)
                  rules (if (re-matches #"[a-z|\"]+" rules-as-string)
                          (clojure.string/replace rules-as-string "\"" "")
                          (->> (clojure.string/split rules-as-string #"\|")
                               (map (fn [r]
                                      (->> (clojure.string/split r #" ")
                                           (remove (fn [x] (= x "")))
                                           (map read-string))))))]

              (assoc a (read-string n) rules)))
          {}
          input))

(defn match-rule
  {:test (fn []
           (is= (match-rule (create-rules test-rules) "aaa" 4) "aa")
           (is= (match-rule (create-rules test-rules) "bab" 4) nil)
           (is= (match-rule (create-rules test-rules) "abaa" 3) "aa")
           (is= (match-rule (create-rules test-rules) "baaa" 3) "aa")
           (is= (match-rule (create-rules test-rules) "aa" 3) nil)
           (is= (match-rule (create-rules test-rules) "ababbb" 0) "")
           (is= (match-rule (create-rules test-rules) "bababa" 0) nil)
           (is= (match-rule (create-rules test-rules) "abbbab" 0) "")
           (is= (match-rule (create-rules test-rules) "aaabbb" 0) nil)
           (is= (match-rule (create-rules test-rules) "aaaabbb" 0) "b"))}
  [rules s rule-id]
  (let [rule (get rules rule-id)]
    (if (string? rule)
      (when (clojure.string/starts-with? s rule)
        (subs s (count rule)))
      (->> rule
           (map (fn [rs]
                  (reduce (fn [s r] (when s (match-rule rules s r)))
                          s
                          rs)))
           (remove nil?)
           (first)))))

(defn exact-match
  {:test (fn []
           (is (exact-match (create-rules test-rules) "ababbb" 0)))}
  [rules s rule]
  (= (match-rule rules s rule) ""))

(deftest puzzle-a
  (is= (let [rules (create-rules (get-puzzle-input-rules))
             messages (get-puzzle-input-messages)]
         (->> messages
              (filter (fn [m] (exact-match rules m 0)))
              (count)))
       291))










