(ns advent-of-code.dec-2016.day-16
  (:require [advent-of-code.test :refer [is= is is-not]]))

(def input "11011110011011101")

(defn step
  {:test (fn []
           (is= (step "1") [\1 \0 \0])
           (is= (step "0") [\0 \0 \1])
           (is= (step "11111") [\1 \1 \1 \1 \1 \0 \0 \0 \0 \0 \0])
           (is= (step "111100001010") (seq "1111000010100101011110000")))}
  [data]
  (concat (seq data) [\0] (map (fn [c] (if (= c \0) \1 \0)) (reverse data))))

(defn checksum
  {:test (fn []
           (is= (checksum "110010110100") (seq "100"))
           (is= (checksum "10000011110010000111") (seq "01100")))}
  [data]
  (if (odd? (count data))
    data
    (recur (->> data
                (partition 2)
                (map (fn [[a b]] (if (= a b) \1 \0)))))))

(defn fill-disc-and-calculate-checksum
  {:test (fn []
           (is= (fill-disc-and-calculate-checksum "10000" 20)
                (seq "01100")))}
  [data size]
  (->> (loop [data data]
         (if (>= (count data) size)
           data
           (recur (step data))))
       (take size)
       (checksum)))

(comment
  (time (apply str (fill-disc-and-calculate-checksum input 272)))
  (time (apply str (fill-disc-and-calculate-checksum input 35651584)))
  ; "Elapsed time: 67801.104077 msecs"
  ; => "00011010100010010"
  )

