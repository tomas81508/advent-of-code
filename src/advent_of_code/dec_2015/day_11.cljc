(ns advent-of-code.dec-2015.day-11
  (:require [ysera.test :refer [deftest is= is is-not]]))


(def puzzle-input "hepxcrrq")

(defn increasing-straight?
  {:test (fn []
           (is (increasing-straight? "acefgza"))
           (is-not (increasing-straight? "acefjga")))}
  [s]
  (loop [[n1 n2 n3 :as ns] (map int s)]
    (cond (not n3)
          false

          (= (+ n1 2) (+ n2 1) n3)
          true

          :else
          (recur (drop 1 ns)))))


(defn illegal-letters?
  {:test (fn []
           (is (illegal-letters? "abciolyz"))
           (is-not (illegal-letters? "abcdefgh")))}
  [s]
  (let [illegal-letters #{\i \o \l}]
    (some illegal-letters s)))


(defn contains-pairs?
  {:test (fn []
           (is (contains-pairs? "abbceffg"))
           (is (contains-pairs? "abcdffaa"))
           (is (contains-pairs? "ghjaabcc"))
           (is-not (contains-pairs? "abcdeggg"))
           (is-not (contains-pairs? "abcdefhh"))
           (is-not (contains-pairs? "hijklmmn"))
           (is-not (contains-pairs? "abbcegjk")))}
  [s]
  (loop [[l1 l2 :as ls] (seq s)
         occurrences 0]
    (cond (and l1 (= l1 l2))
          (if (zero? occurrences)
            (recur (drop 2 ls) (inc occurrences))
            true)

          (not l2)
          false

          :else
          (recur (drop 1 ls) occurrences))))


(defn password?
  {:test (fn []
           (is-not (password? "hijklmmn"))
           (is-not (password? "abbceffg"))
           (is-not (password? "abbcegjk"))
           (is (password? "abcdffaa"))
           (is (password? "ghjaabcc")))}
  [s]
  (and (increasing-straight? s)
       (not (illegal-letters? s))
       (contains-pairs? s)))


(defn next-candidate
  {:test (fn []
           (is= (next-candidate [\a \b \c])
                [\a \b \d])
           (is= (next-candidate [\a \b \z \z])
                [\a \c \a \a]))}
  [s]
  (loop [ls s
         index (dec (count s))]
    (let [c (get ls index)]
      (if (not= c \z)
        (update ls index (fn [c] (char (inc (int c)))))
        (recur (assoc ls index \a)
               (dec index))))))


(defn next-password
  {:test (fn []
           (is= (next-password "abcdefgh") "abcdffaa")
           (is= (next-password "ghijklmn") "ghjaabcc")
           )}
  [s]
  (loop [candidate (next-candidate (into [] (seq s)))]
    (if (password? candidate)
      (clojure.string/join candidate)
      (recur (next-candidate candidate)))))


(deftest puzzle-a
         (is= (time (next-password "hepxcrrq"))
              ; "Elapsed time: 1176.312849 msecs"
              "hepxxyzz"))

(deftest puzzle-b
         (is= (time (next-password "hepxxyzz"))
              ; "Elapsed time: 3094.955637 msecs"
              "heqaabcc"))