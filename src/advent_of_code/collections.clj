(ns advent-of-code.collections
  (:require [advent-of-code.test :refer [is is=]]))

(defn index-of
  "Gets the index of the given element of the collection."
  {:test (fn []
           (is= (index-of ["a" "b" "c"] "b")
                1)
           (is= (index-of ["a" "b" "c"] "z")
                nil)
           (is= (index-of [] "b")
                nil))}
  [coll x]
  (first (keep-indexed (fn [y z] (when (= z x) y)) coll)))

(defn seq-contains?
  "Determines if the given element is contained in the given collection."
  {:test (fn []
           (is (seq-contains? ["a" "b" "c"] "a")))}
  [coll x]
  (not (nil? (index-of coll x))))
