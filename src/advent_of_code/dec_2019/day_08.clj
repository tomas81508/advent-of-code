(ns advent-of-code.dec-2019.day-08
  (:require [ysera.test :refer [is is-not is= deftest]]))

(defn get-puzzle-input []
  (as-> (slurp "src/advent_of_code/dec_2019/day_08.txt") $
        (map (fn [n] (read-string (str n))) $)))

(deftest puzzle-a
         (is= (as-> (get-puzzle-input) $
                    (partition 150 $)
                    (map (fn [layer]
                           (frequencies layer)) $)
                    (reduce (fn [result layer]
                              (if (or (nil? result)
                                      (< (get layer 0) (get result 0)))
                                layer
                                result))
                            nil
                            $)
                    (* (get $ 1) (get $ 2)))
              2760))


(deftest puzzle-b
         (is= (as-> (get-puzzle-input) $
                    (partition 25 $)
                    (partition 6 $)
                    (reduce (fn [result layer]
                              (map (fn [result-row layer-row]
                                     (map (fn [result-cell layer-cell]
                                            (if (= result-cell 2)
                                              layer-cell
                                              result-cell))
                                          result-row
                                          layer-row))
                                   result
                                   layer))
                            $)
                    (map (fn [row] (->> row
                                        (map (fn [c] (if (zero? c) " " "*")))
                                        (clojure.string/join "")))
                         $))
              [" **   **  *  * **** ***  "
               "*  * *  * *  * *    *  * "
               "*  * *    *  * ***  ***  "
               "**** * ** *  * *    *  * "
               "*  * *  * *  * *    *  * "
               "*  *  ***  **  **** ***  "]))
