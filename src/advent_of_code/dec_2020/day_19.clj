(ns advent-of-code.dec-2020.day-19
  (:require [ysera.test :refer [is is-not is= deftest]]))

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

(declare match-rule)

(defn match-rules
  {:test (fn []
           (is= (match-rules (create-rules test-rules) ["aaa" "a" "aaaaa"] [4 4]) ["a" "aaa"])
           (is= (match-rules (create-rules test-rules) ["abaa"] [4 5]) ["aa"])
           (is= (match-rules (create-rules test-rules) ["abaa"] [5 4]) nil))}
  [rules messages matching-rules]
  (reduce (fn [messages m-r]
            (let [result (->> messages
                              (map (fn [m] (match-rule rules m m-r)))
                              (remove nil?)
                              (flatten))]
              (when-not (empty? result) result)))
          messages
          matching-rules))

(defn match-rule
  {:test (fn []
           (is= (match-rule (create-rules test-rules) "aaa" 4) ["aa"])
           (is= (match-rule (create-rules test-rules) "bab" 4) nil)
           (is= (match-rule (create-rules test-rules) "abaa" 3) ["aa"])
           (is= (match-rule (create-rules test-rules) "baaa" 3) ["aa"])
           (is= (match-rule (create-rules test-rules) "aa" 3) nil)
           (is= (match-rule (create-rules test-rules) "ababbb" 0) [""])
           (is= (match-rule (create-rules test-rules) "bababa" 0) nil)
           (is= (match-rule (create-rules test-rules) "abbbab" 0) [""])
           (is= (match-rule (create-rules test-rules) "aaabbb" 0) nil)
           (is= (match-rule (create-rules test-rules) "aaaabbb" 0) ["b"])
           (is= (match-rule (create-rules test-rules) "aaaab" 0) nil)
           (is= (match-rule {0 "a" 1 "aa" 2 [[0] [1]]} "aaa" 2) ["aa" "a"]))}
  [rules message rule-id]
  (let [rule (get rules rule-id)]
    (if (string? rule)
      (when (clojure.string/starts-with? message rule)
        [(subs message (count rule))])
      (let [result (->> rule
                        (map (fn [r-part] (match-rules rules [message] r-part)))
                        (remove nil?)
                        (flatten))]
        (when-not (empty? result) result)))))

(defn exact-match
  {:test (fn []
           (is (exact-match (create-rules test-rules) "ababbb" 0)))}
  [rules s rule]
  (some (fn [result] (= result ""))
        (match-rule rules s rule)))

(deftest puzzle-a
         (is= (time (let [rules (create-rules (get-puzzle-input-rules))
                          messages (get-puzzle-input-messages)]
                      (->> messages
                           (filter (fn [m] (exact-match rules m 0)))
                           (count))))
              ; "Elapsed time: 308.766772 msecs"
              291)
         )

(def test-rules-2 (create-rules ["0: 8 11"
                                 "1: \"a\""
                                 "2: 1 24 | 14 4"
                                 "3: 5 14 | 16 1"
                                 "4: 1 1"
                                 "5: 1 14 | 15 1"
                                 "6: 14 14 | 1 14"
                                 "7: 14 5 | 1 21"
                                 "8: 42"
                                 "9: 14 27 | 1 26"
                                 "10: 23 14 | 28 1"
                                 "11: 42 31"
                                 "12: 24 14 | 19 1"
                                 "13: 14 3 | 1 12"
                                 "14: \"b\""
                                 "15: 1 | 14"
                                 "16: 15 1 | 14 14"
                                 "17: 14 2 | 1 7"
                                 "18: 15 15"
                                 "19: 14 1 | 14 14"
                                 "20: 14 14 | 1 15"
                                 "21: 14 1 | 1 14"
                                 "22: 14 14"
                                 "23: 25 1 | 22 14"
                                 "24: 14 1"
                                 "25: 1 1 | 1 14"
                                 "26: 14 22 | 1 20"
                                 "27: 1 6 | 14 18"
                                 "28: 16 1"
                                 "31: 14 17 | 1 13"
                                 "42: 9 14 | 10 1"]))

(def test-rules-2-modified (-> test-rules-2
                               (assoc 8 [[42] [42 8]]
                                      11 [[42 31] [42 11 31]])))

(deftest test-2
         (is-not (exact-match test-rules-2 "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa" 0))
         (is (exact-match test-rules-2 "bbabbbbaabaabba" 0))
         (is-not (exact-match test-rules-2 "babbbbaabbbbbabbbbbbaabaaabaaa" 0))
         (is-not (exact-match test-rules-2 "aaabbbbbbaaaabaababaabababbabaaabbababababaaa" 0))
         (is-not (exact-match test-rules-2 "bbbbbbbaaaabbbbaaabbabaaa" 0))
         (is-not (exact-match test-rules-2 "bbbababbbbaaaaaaaabbababaaababaabab" 0))
         (is (exact-match test-rules-2 "ababaaaaaabaaab" 0))
         (is (exact-match test-rules-2 "ababaaaaabbbaba" 0))
         (is-not (exact-match test-rules-2 "baabbaaaabbaaaababbaababb" 0))
         (is-not (exact-match test-rules-2 "abbbbabbbbaaaababbbbbbaaaababb" 0))
         (is-not (exact-match test-rules-2 "aaaaabbaabaaaaababaa" 0))
         (is-not (exact-match test-rules-2 "aaaabbaaaabbaaa" 0))
         (is-not (exact-match test-rules-2 "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa" 0))
         (is-not (exact-match test-rules-2 "babaaabbbaaabaababbaabababaaab" 0))
         (is-not (exact-match test-rules-2 "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba" 0)))

(deftest test-puzzle-b
         (is-not (exact-match test-rules-2-modified "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa" 0))
         (is (exact-match test-rules-2-modified "bbabbbbaabaabba" 0))
         (is (exact-match test-rules-2-modified "babbbbaabbbbbabbbbbbaabaaabaaa" 0))
         (is (exact-match test-rules-2-modified "aaabbbbbbaaaabaababaabababbabaaabbababababaaa" 0))
         (is (exact-match test-rules-2-modified "bbbbbbbaaaabbbbaaabbabaaa" 0))
         (is (exact-match test-rules-2-modified "bbbababbbbaaaaaaaabbababaaababaabab" 0))
         (is (exact-match test-rules-2-modified "ababaaaaaabaaab" 0))
         (is (exact-match test-rules-2-modified "ababaaaaabbbaba" 0))
         (is (exact-match test-rules-2-modified "baabbaaaabbaaaababbaababb" 0))
         (is (exact-match test-rules-2-modified "abbbbabbbbaaaababbbbbbaaaababb" 0))
         (is (exact-match test-rules-2-modified "aaaaabbaabaaaaababaa" 0))
         (is-not (exact-match test-rules-2-modified "aaaabbaaaabbaaa" 0))
         (is (exact-match test-rules-2-modified "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa" 0))
         (is-not (exact-match test-rules-2-modified "babaaabbbaaabaababbaabababaaab" 0))
         (is (exact-match test-rules-2-modified "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba" 0))
         )

(deftest puzzle-b
         (is= (time (let [rules (-> (create-rules (get-puzzle-input-rules))
                                    (assoc 8 [[42] [42 8]]
                                           11 [[42 31] [42 11 31]]))
                          messages (get-puzzle-input-messages)]
                      (->> messages
                           (map-indexed (fn [i m] [i m]))
                           (filter (fn [[i m]]
                                     (exact-match rules m 0)))
                           (count))))
              ; "Elapsed time: 2231.075036 msecs"
              409)
         )








