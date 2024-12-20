(ns advent-of-code.dec-2017.day-09
  (:require [advent-of-code.test :refer [is=]]
            [clojure.string :refer [join]]))

(def test-input ["{}"
                 "{{{}}}"
                 "{{},{}}"
                 "{{{},{},{{}}}}"
                 "{<a>,<a>,<a>,<a>}"
                 "{{<ab>},{<ab>},{<ab>},{<ab>}}"
                 "{{<!!>},{<!!>},{<!!>},{<!!>}}"
                 "{{<a!>},{<a!>},{<a!>},{<ab>}}"])

(defn remove-ignored
  {:test (fn []
           (is= (join (remove-ignored "{{<ab>},{<ab>},{<ab>},{<ab>}}"))
                "{{<ab>},{<ab>},{<ab>},{<ab>}}")
           (is= (join (remove-ignored "{{<!!>},{<!!>},{<!!>},{<!!>}}"))
                "{{<>},{<>},{<>},{<>}}")
           (is= (join (remove-ignored "{{<a!>},{<a!>},{<a!>},{<ab>}}"))
                "{{<a},{<a},{<a},{<ab>}}"))}
  [line]
  (loop [[f & r] (seq line)
         result []]
    (cond (nil? f) result
          (= f \!) (recur (drop 1 r) result)
          :else (recur r (conj result f)))))

(defn remove-garbage
  {:test (fn []
           (is= (join (remove-garbage "{{<ab>},{<ab>},{<ab>},{<ab>}}"))
                "{{},{},{},{}}")
           (is= (join (remove-garbage "{{<>},{<>},{<>},{<>}}"))
                "{{},{},{},{}}")
           (is= (join (remove-garbage "{{<a},{<a},{<a},{<ab>}}"))
                "{{}}"))}
  [line]
  (loop [[f & r] (seq line)
         inside-garbage false
         result []]
    (cond (nil? f) result

          (and (= f \<) (not inside-garbage))
          (recur r true result)

          (and (= f \>) inside-garbage)
          (recur r false result)

          inside-garbage
          (recur r true result)

          :else
          (recur r false (conj result f)))))

(defn score
  {:test (fn []
           (is= (score "{}") 1)
           (is= (score "{{{}}}") 6)
           (is= (score "{{},{}}") 5)
           (is= (score "{{{},{},{{}}}}") 16)
           (is= (score "{<a>,<a>,<a>,<a>}") 1)
           (is= (score "{{<ab>},{<ab>},{<ab>},{<ab>}}") 9)
           (is= (score "{{<!!>},{<!!>},{<!!>},{<!!>}}") 9)
           (is= (score "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3)
           )}
  [line]
  (let [trimmed-line (-> line
                         (remove-ignored)
                         (remove-garbage))]
    (loop [[f & r] trimmed-line
           inside-number 0
           result 0]
      (cond (nil? f) result
            (= f \{) (recur r (inc inside-number) result)
            (= f \}) (recur r (dec inside-number) (+ result inside-number))
            :else (recur r inside-number result)))))

(def input (slurp "src/advent_of_code/dec_2017/day_09_input.txt"))

(comment
  (score input)
  17390
  )

(defn count-garbage
  {:test (fn []
           (is= (count-garbage "<>") 0)
           (is= (count-garbage "<random characters>") 17)
           (is= (count-garbage "<<<<>") 3)
           (is= (count-garbage "<{!>}>") 2)
           (is= (count-garbage "<!!>") 0)
           (is= (count-garbage "<!!!>>") 0)
           (is= (count-garbage "<{o\"i!a,<{i<a>") 10))}
  [line]
  (let [trimmed-line (remove-ignored line)]
    (loop [[f & r] trimmed-line
           inside-garbage false
           result 0]
      (cond (nil? f) result

            (and (= f \<) (not inside-garbage))
            (recur r true result)

            (and (= f \>) inside-garbage)
            (recur r false result)

            inside-garbage
            (recur r true (inc result))

            :else
            (recur r false result)))))

(comment
  (count-garbage input)
  7825
  )