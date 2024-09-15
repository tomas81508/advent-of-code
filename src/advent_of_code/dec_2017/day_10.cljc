(ns advent-of-code.dec-2017.day-10
  (:require [ysera.test :refer [is=]]))

(def input-string "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24")

(def input (as-> input-string $
                 (clojure.string/split $ #",")
                 (map read-string $)
                 (into [] $)))

(def test-input [3 4 1 5])

(def test-state {:numbers [0 1 2 3 4] :skip-size 0 :list-start-index 0})

(defn apply-length
  {:test (fn []
           (is= (apply-length test-state 3 5)
                {:numbers [3 4 2 1 0] :skip-size 1 :list-start-index 2})
           (is= (apply-length {:numbers [3 4 2 1 0] :skip-size 1 :list-start-index 2} 4 5)
                {:numbers [1 2 4 3 0] :skip-size 2 :list-start-index 2})
           (is= (apply-length {:numbers [1 2 4 3 0] :skip-size 2 :list-start-index 2} 1 5)
                {:numbers [3 0 1 2 4] :skip-size 3 :list-start-index 4})
           (is= (apply-length {:numbers [3 0 1 2 4] :skip-size 3 :list-start-index 4} 5 5)
                {:numbers [0 3 4 2 1] :skip-size 4 :list-start-index 1}))}
  [state length n]
  (let [[part-a part-b] (split-at length (:numbers state))
        numbers (cycle (concat (reverse part-a) part-b))
        list-move (rem (+ length (:skip-size state)) n)]
    {:numbers          (->> numbers
                            (drop list-move)
                            (take n))
     :skip-size        (inc (:skip-size state))
     :list-start-index (rem (+ (:list-start-index state) (- n list-move)) n)}))

(defn apply-lengths
  {:test (fn []
           (is= (apply-lengths test-state test-input)
                {:numbers [0 3 4 2 1] :skip-size 4 :list-start-index 1}))}
  [state lengths]
  (let [n (count (:numbers state))]
    (reduce (fn [state length] (apply-length state length n))
            state
            lengths)))

(defn solve-puzzle-a
  {:test (fn []
           (is= (solve-puzzle-a {:numbers [0 3 4 2 1] :skip-size 4 :list-start-index 1})
                12))}
  [state]
  (->> (cycle (:numbers state))
       (drop (:list-start-index state))
       (take 2)
       (reduce *)))

(def state {:numbers (range 256) :skip-size 0 :list-start-index 0})

(comment
  (->> (apply-lengths state input)
       (solve-puzzle-a))
  4480
  )

(defn get-ascii-number
  {:test (fn []
           (is= (get-ascii-number \1) 49))}
  [char]
  (int char))

(defn string->ascii
  {:test (fn []
           (is= (string->ascii "1,2,3") [49 44 50 44 51]))}
  [s]
  (map get-ascii-number s))

(defn get-sequence-of-length
  {:test (fn []
           (is= (get-sequence-of-length "1,2,3")
                [49, 44, 50, 44, 51, 17, 31, 73, 47, 23]))}
  [input]
  (concat (string->ascii input)
          [17 31 73 47 23]))

(defn get-sparse-hash
  [state lengths]
  (apply-lengths state (flatten (repeat 64 lengths))))

(comment
  (get-sparse-hash state (get-sequence-of-length input-string))
  )

(defn xor
  {:test (fn []
           (is= (xor [65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22]) 64))}
  [numbers]
  (reduce bit-xor numbers))

(defn dense-hash
  [sparse-hash]
  (->> (partition 16 sparse-hash)
       (map xor)))

(def hex {0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "a" 11 "b" 12 "c" 13 "d" 14 "e" 15 "f"})

(defn to-hexadecimal
  {:test (fn []
           (is= (to-hexadecimal 0) "00")
           (is= (to-hexadecimal 7) "07")
           (is= (to-hexadecimal 64) "40")
           (is= (to-hexadecimal 255) "ff"))}
  [n]
  (str (hex (quot n 16)) (hex (rem n 16))))

(defn knot-hash
  {:test (fn []
           (is= (knot-hash "") "a2582a3a0e66e6e86e3812dcb672a272")
           (is= (knot-hash "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd")
           (is= (knot-hash "1,2,3") "3efbe78a8d82f29979031a4aa0b16a9d")
           (is= (knot-hash "1,2,4") "63960835bcdc130f0b66d7ff4f6a5a8e"))}
  [input-string]
  (let [sparse-result (get-sparse-hash state (get-sequence-of-length input-string))]
    (->> (:numbers sparse-result)
         (cycle)
         (drop (:list-start-index sparse-result))
         (take 256)
         (dense-hash)
         (map to-hexadecimal)
         (apply str)
         )))


(comment
  (knot-hash input-string)
  ; wrong answer "fad2e4b7d59dabc4c500ffe015c83b60"

  )