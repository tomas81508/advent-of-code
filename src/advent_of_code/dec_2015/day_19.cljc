(ns advent-of-code.dec-2015.day-19
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.edn :as edn]))

(def replacements (->> (slurp "src/advent_of_code/dec_2015/day_19_input-1.txt")
                       (clojure.string/split-lines)
                       (reduce (fn [a s]
                                 (let [[_ f r] (re-find #"(\w+) => (\w+)" s)]
                                   (update a f conj r)))
                               {})))

(def molecule (slurp "src/advent_of_code/dec_2015/day_19_input-2.txt"))

(def test-replacements {"H" ["HO" "OH"] "O" ["HH"]})
(def test-molecule "HOH")

(defn find-next-replacement
  {:test (fn []
           (is= (find-next-replacement "abcHi20" {"Hi" ["G"]})
                ["Hi" 3])
           (is= (find-next-replacement "abc" {"Hi" ["G"]})
                nil))}
  ([molecule replacements] (find-next-replacement molecule replacements 0))
  ([molecule replacements index]
   (when-not (empty? molecule)
     (let [c (subs molecule 0 1)]
       (if (contains? replacements c)
         [c index]
         (when (> (count molecule) 1)
           (let [c (subs molecule 0 2)]
             (if (contains? replacements c)
               [c index]
               (recur (subs molecule 1) replacements (inc index))))))))))

(defn do-replacements
  {:test (fn []
           (is= (do-replacements test-molecule test-replacements)
                #{"HOOH" "HOHO" "OHOH" "HHHH"}))}
  [molecule replacements]
  (loop [handled-part ""
         unhandled-part molecule
         results #{}]
    (let [r (find-next-replacement unhandled-part replacements)]
      (if-not r
        results
        (let [[replace-key index] r
              replace-values (replacements replace-key)]
          (recur (str handled-part (subs unhandled-part 0 (+ index (count replace-key))))
                 (subs unhandled-part (+ index (count replace-key)))
                 (reduce (fn [a v]
                           (conj a (str handled-part
                                        (subs unhandled-part 0 index)
                                        v
                                        (subs unhandled-part (+ index (count replace-key))))))
                         results
                         replace-values)))))))

(comment
  (time (count (do-replacements molecule replacements)))
  )

; part 2

(def test-replacements-2 {"e" ["H" "O"] "H" ["HO" "OH"] "O" ["HH"]})

(defn invers-replacements
  {:test (fn []
           (is= (invers-replacements test-replacements-2)
                {"H"  "e"
                 "O"  "e"
                 "HO" "H"
                 "OH" "H"
                 "HH" "O"}))}
  [replacements]
  (reduce-kv (fn [a k v]
               (reduce (fn [a v]
                         (assoc a v k))
                       a
                       v))
             {}
             replacements))

(defn solve-part-2
  [molecule replacements]
  (let [inv-repl (invers-replacements replacements)]
    (loop [molecule molecule
           steps 0]
      (let [[new-molecule steps] (reduce (fn [[molecule steps] [v k]]
                                           (let [new-molecule (clojure.string/replace-first molecule v k)]
                                             (if (not= new-molecule molecule)
                                               [new-molecule (inc steps)]
                                               [molecule steps])))
                                         [molecule steps]
                                         inv-repl)]
        (if (= new-molecule "e")
          steps
          (recur new-molecule
                 steps))))))

(comment
  (solve-part-2 molecule replacements)
  )