(ns advent-of-code.dec-2021.day-14
  (:require [advent-of-code.test :refer [is=]]
                       [clojure.test :refer [deftest]])
  (:import (java.util ArrayList)))

(def pair-insertion-pattern #"(\w\w) -> (\w)")

(def test-template (seq "NNCB"))
(def test-pair-insertions
  (->> ["CH -> B" "HH -> N" "CB -> H" "NH -> C" "HB -> C" "HC -> B" "HN -> C" "NN -> C" "BH -> H" "NC -> B" "NB -> B" "BN -> B" "BB -> N" "BC -> B" "CC -> N" "CN -> C"]
       (map (fn [pair-insertion]
              (let [[_ pair insertion] (re-matches pair-insertion-pattern pair-insertion)]
                [(seq pair) (first (seq insertion))])))
       (into {})))

(def puzzle-template (seq "BNBBNCFHHKOSCHBKKSHN"))
(def puzzle-pair-insertions
  (->> ["CH -> S" "KK -> V" "FS -> V" "CN -> P" "VC -> N" "CB -> V" "VK -> H" "CF -> N" "PO -> O" "KC -> S" "HC -> P" "PP -> B" "KO -> B" "BK -> P" "BH -> N" "CC -> N" "PC -> O" "FK -> N" "KF -> F" "FH -> S" "SS -> V" "ON -> K" "OV -> K" "NK -> H" "BO -> C" "VP -> O" "CS -> V" "KS -> K" "SK -> B" "OP -> S" "PK -> S" "HF -> P" "SV -> P" "SB -> C" "BC -> C" "FP -> H" "FC -> P" "PB -> N" "NV -> F" "VO -> F" "VH -> P" "BB -> N" "SF -> F" "NB -> K" "KB -> S" "VV -> S" "NP -> N" "SO -> O" "PN -> B" "BP -> H" "BV -> V" "OB -> C" "HV -> N" "PF -> B" "SP -> N" "HN -> N" "CV -> H" "BN -> V" "PS -> V" "CO -> S" "BS -> N" "VB -> H" "PV -> P" "NN -> P" "HS -> C" "OS -> P" "FB -> S" "HO -> C" "KH -> H" "HB -> K" "VF -> S" "CK -> K" "FF -> H" "FN -> P" "OK -> F" "SC -> B" "HH -> N" "OH -> O" "VS -> N" "FO -> N" "OC -> H" "NF -> F" "PH -> S" "HK -> K" "NH -> H" "FV -> S" "OF -> V" "NC -> O" "HP -> O" "KP -> B" "BF -> N" "NO -> S" "CP -> C" "NS -> N" "VN -> K" "KV -> N" "OO -> V" "SN -> O" "KN -> C" "SH -> F"]
       (map (fn [pair-insertion]
              (let [[_ pair insertion] (re-matches pair-insertion-pattern pair-insertion)]
                [(seq pair) (first (seq insertion))])))
       (into {})))

(defn step
  {:test (fn []
           (is= (step test-pair-insertions test-template)
                [\N \C \N \B \C \H \B])
           (is= (->> test-template
                     ((apply comp (repeat 4 (partial step test-pair-insertions)))))
                [\N \B \B \N \B \N \B \B \C \C \N \B \C \N \C \C \N \B \B \N \B \B \N \B \B \B \N \B \B \N \B \B \C \B \H \C \B \H \H \N \H \C \B \B \C \B \H \C \B]))}
  [pair-insertions template]
  (loop [[f & r] template
         result []]
    (let [s (first r)]
      (if-not s
        (conj result f)
        (recur r
               (if-let [i (get pair-insertions [f s])]
                 (conj result f i)
                 (conj result f)))))))

(deftest test-solver
         (is= (let [freq (->> test-template
                              ((apply comp (repeat 10 (partial step test-pair-insertions))))
                              (frequencies)
                              (seq)
                              (map second)
                              (sort))]
                (- (last freq) (first freq)))
              1588))

(comment
  ;; Puzzle part A
  (time (let [freq (->> puzzle-template
                        ((apply comp (repeat 10 (partial step puzzle-pair-insertions))))
                        (frequencies)
                        (seq)
                        (map second)
                        (sort))]
          (- (last freq) (first freq))))
  ; "Elapsed time: 16.182295 msecs"
  3009)

; Part b needs a different approach

(defn add-combination
  [combinations combination amount]
  (if (contains? combinations combination)
    (update combinations combination (fn [v] (+ v amount)))
    (assoc combinations combination amount)))

(defn create-combinations
  {:test (fn []
           (is= (create-combinations "NNCB")
                {[\N \N] 1
                 [\N \C] 1
                 [\C \B] 1}))}
  [template]
  (->> template
       (partition 2 1)
       ;(map (fn [x] (clojure.string/join x)))
       (reduce (fn [a v] (add-combination a v 1)) {})))

(defn step-2
  {:test (fn []
           (is= (->> test-template
                     (create-combinations)
                     (step-2 test-pair-insertions))
                {[\N \C] 1, [\C \N] 1, [\N \B] 1, [\B \C] 1, [\C \H] 1, [\H \B] 1}))}
  [pair-insertions combinations]
  (reduce-kv (fn [a combination amount]
               (if-let [insertion (get pair-insertions combination)]
                 (-> a
                     (add-combination [(first combination) insertion] amount)
                     (add-combination [insertion (second combination)] amount))
                 (add-combination a combination amount)))
             {}
             combinations))

(comment
  (time (let [double-freq (->> puzzle-template
                               (create-combinations)
                               ((apply comp (repeat 10 (partial step-2 puzzle-pair-insertions))))
                               (reduce-kv (fn [a [c1 c2] v]
                                            (-> a
                                                (update c1 (fn [x] (+ (or x 0) v)))
                                                (update c2 (fn [x] (+ (or x 0) v)))))
                                          {})
                               (vals)
                               (sort))
              f (first double-freq)
              min (/ (if (odd? f) (inc f) f) 2)
              l (last double-freq)
              max (/ (if (odd? l) (inc l) l) 2)]
          (- max min)))
  ; "Elapsed time: 3.087743 msecs"
  3009


  (time (let [double-freq (->> puzzle-template
                               (create-combinations)
                               ((apply comp (repeat 40 (partial step-2 puzzle-pair-insertions))))
                               (reduce-kv (fn [a [c1 c2] v]
                                            (-> a
                                                (update c1 (fn [x] (+ (or x 0) v)))
                                                (update c2 (fn [x] (+ (or x 0) v)))))
                                          {})
                               (vals)
                               (sort))
              f (first double-freq)
              min (/ (if (odd? f) (inc f) f) 2)
              l (last double-freq)
              max (/ (if (odd? l) (inc l) l) 2)]
          (- max min)))
  ; "Elapsed time: 9.732555 msecs"
  3459822539451)


