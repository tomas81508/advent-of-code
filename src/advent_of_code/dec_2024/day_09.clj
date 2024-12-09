(ns advent-of-code.dec-2024.day-09
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.math.combinatorics :refer [combinations]]))

(def input (slurp "src/advent_of_code/dec_2024/day_09_input.txt"))

(def test-input-1 "12345")
(def test-input-2 "2333133121414131402")

(defn get-first-available-index-from
  [memory start-index]
  (if (>= start-index (count memory))
    :index-out-of-bound
    (if (nil? (get memory start-index))
      start-index
      (recur memory (inc start-index)))))

(defn get-last-non-available-index-from
  [memory end-index]
  (if (neg? end-index)
    :index-out-of-bound
    (if (nil? (get memory end-index))
      (recur memory (dec end-index))
      end-index)))


(defn create-state
  {:test (fn []
           (is= (create-state test-input-1)
                {:memory                   [0 nil nil 1 1 1 nil nil nil nil 2 2 2 2 2]
                 :first-available-index    1
                 :last-non-available-index 14})
           (is= (create-state test-input-2)
                {:memory                   [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9]
                 :first-available-index    2
                 :last-non-available-index 41}))}
  [input]
  (let [memory (->> input
                    (seq)
                    (map (comp read-string str))
                    (into [])
                    (reduce-kv (fn [a index v]
                                 (if (even? index)
                                   (reduce conj a (repeat v (/ index 2)))
                                   (reduce conj a (repeat v nil))))
                               [])
                    (into []))]
    {:memory                   memory
     :first-available-index    (get-first-available-index-from memory 0)
     :last-non-available-index (get-last-non-available-index-from memory (dec (count memory)))}))

(def state (create-state input))

(defn swap-memory
  {:test (fn []
           (is= (swap-memory (create-state test-input-1))
                {:memory                   [0 2 nil 1 1 1 nil nil nil nil 2 2 2 2 nil]
                 :first-available-index    2
                 :last-non-available-index 13}))}
  [{memory                   :memory
    first-available-index    :first-available-index
    last-non-available-index :last-non-available-index
    :as                      state}]
  (-> state
      (update :memory (fn [memory] (-> memory
                                       (assoc first-available-index (get memory last-non-available-index))
                                       (assoc last-non-available-index nil))))
      (assoc :first-available-index (get-first-available-index-from memory (inc first-available-index)))
      (assoc :last-non-available-index (get-last-non-available-index-from memory (dec last-non-available-index)))))

(defn move-all-blocks
  [state]
  (loop [state state]
    (if (> (:first-available-index state) (:last-non-available-index state))
      state
      (recur (swap-memory state)))))

(def test-result-2 (move-all-blocks (create-state test-input-2)))

(def result (move-all-blocks state))

(defn checksum
  {:test (fn []
           (is= (checksum test-result-2)
                1928))}
  [{memory :memory}]
  (->> memory
       (map-indexed (fn [index value] (if value (* index value) 0)))
       (reduce +)))

(comment
  (checksum result)
  ; 6346871685398
  )

(defn get-last-file
  {:test (fn []
           (is= (get-last-file [0 0 9 9 nil 1 1 1 7 7 7 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil nil nil nil nil 8 8 8 8 nil nil]
                               40)
                ; number starting index
                [8 36 39]))}
  [memory index]
  (loop [stacking nil
         index index
         end-index nil]
    (let [v (get memory index)]
      (cond (neg? index) :no-file
            (and (nil? v) (nil? stacking)) (recur nil (dec index) nil)
            (nil? v) [stacking (inc index) end-index]
            (nil? stacking) (recur v (dec index) index)
            (not= v stacking) [stacking (inc index) end-index]
            :else (recur stacking (dec index) end-index)))))

(defn find-space
  {:test (fn []
           (is= (find-space [0 0 9 9 nil 1 1 1 7 7 7 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil nil nil nil nil 8 8 8 8 nil nil]
                            3
                            40
                            2)
                12))}
  [memory start-index end-index size]
  (loop [counter 0
         index start-index]
    (let [v (get memory (+ index counter))]
      (cond (> index end-index) :nothing
            (= counter size) index
            (nil? v) (recur (inc counter) index)
            :else (recur 0 (inc (+ index counter)))))))


(defn move-file
  {:test (fn []
           (is= (move-file [0 0 9 9 nil 1 1 1 7 7 7 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil nil nil nil nil 8 8 8 8 nil nil]
                           [4 19 20]
                           12)
                [0 0 9 9 nil 1 1 1 7 7 7 2 4 4 nil 3 3 3 nil nil nil nil 5 5 5 5 nil 6 6 6 6 nil nil nil nil nil 8 8 8 8 nil nil]))}
  [memory [v start end] space-start]
  (let [size (inc (- end start))
        file-indexes (range start (inc end))
        space-indexes (range space-start (+ space-start size))]
    (as-> memory $
          (apply assoc $ (->> file-indexes
                              (map (fn [index] [index nil]))
                              (flatten)))
          (apply assoc $ (->> space-indexes
                              (map (fn [index] [index v]))
                              (flatten))))))

(defn move-files
  {:test (fn []
           (is= (move-files (:memory (create-state test-input-2)))
                [0 0 9 9 2 1 1 1 7 7 7 nil 4 4 nil 3 3 3 nil nil nil nil 5 5 5 5 nil 6 6 6 6 nil nil nil nil nil 8 8 8 8 nil nil]))}
  [memory]
  (loop [memory memory
         index (dec (count memory))]
    (let [result (get-last-file memory index)]
      (if (= result :no-file)
        memory
        (let [[_ start-index end-index] result
              size (inc (- end-index start-index))
              empty-space-index (find-space memory 0 start-index size)]
          (if (= :nothing empty-space-index)
            (recur memory (dec start-index))
            (recur (move-file memory result empty-space-index) (dec start-index))))))))

(comment
  (def fragmented (move-files (:memory state)))
  (checksum {:memory fragmented})
  ; 6373055193464

  )


