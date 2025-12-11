(ns advent-of-code.dec-2025.day-11
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.string :as string]
            [clojure.set :as clojure-set]))

; together with James Trunk

(def input (->> (slurp "src/advent_of_code/dec_2025/day_11_input.txt")
                (string/split-lines)))

(def test-input ["aaa: you hhh"
                 "you: bbb ccc"
                 "bbb: ddd eee"
                 "ccc: ddd eee fff"
                 "ddd: ggg"
                 "eee: out"
                 "fff: out"
                 "ggg: out"
                 "hhh: ccc fff iii"
                 "iii: out"])

(def test-input-2 ["svr: aaa bbb"
                   "aaa: fft"
                   "fft: ccc"
                   "bbb: tty"
                   "tty: ccc"
                   "ccc: ddd eee"
                   "ddd: hub"
                   "hub: fff"
                   "eee: dac"
                   "dac: fff"
                   "fff: ggg hhh"
                   "ggg: out"
                   "hhh: out"])

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:aaa [:you :hhh]
                 :you [:bbb :ccc]
                 :bbb [:ddd :eee]
                 :ccc [:ddd :eee :fff]
                 :ddd [:ggg]
                 :eee [:out]
                 :fff [:out]
                 :ggg [:out]
                 :hhh [:ccc :fff :iii]
                 :iii [:out]}))}
  [input]
  (reduce (fn [acc r] (let [[key routes-string] (string/split r #":")
                            routes (re-seq #"\w+" routes-string)]
                        (assoc acc (keyword key) (map keyword routes))))
          {}
          input))

(defn get-paths
  {:test (fn []
           (is= (get-paths (create-state test-input) :you :out)
                #{[:you :bbb :ddd :ggg :out]
                  [:you :bbb :eee :out]
                  [:you :ccc :ddd :ggg :out]
                  [:you :ccc :eee :out]
                  [:you :ccc :fff :out]}))}
  [state start-device stop-device]
  (loop [out-paths #{}
         ongoing-paths #{[start-device]}]
    (if (empty? ongoing-paths)
      (set out-paths)
      (let [new-paths (->> ongoing-paths
                           (mapcat (fn [path] (let [current-device (last path)]
                                                (->> (get state current-device)
                                                     (reduce (fn [acc d]
                                                               (conj acc (conj path d)))
                                                             []))))))
            {new-out-paths     true
             new-ongoing-paths false} (group-by (fn [path] (= (last path) stop-device)) new-paths)]
        (recur (concat out-paths new-out-paths)
               new-ongoing-paths)))))

(declare number-of-paths)
(defn number-of-paths-raw
  {:test (fn []
           (is= (number-of-paths (create-state test-input) :you :out) 5)
           (is= (number-of-paths (create-state test-input-2) :dac :fft) 0)
           (is= (number-of-paths (create-state test-input-2) :fft :dac) 1)
           (is= (number-of-paths (create-state test-input-2) :svr :fft) 1)
           (is= (number-of-paths (create-state test-input-2) :dac :out) 2)
           (is= (number-of-paths (create-state test-input-2) :svr :out) 8))}
  [state device-1 device-2]
  (let [next-devices (get state device-1)]
    (apply + (map (fn [nd] (if (= nd device-2) 1 (number-of-paths state nd device-2)))
                  next-devices))))
(def number-of-paths (memoize number-of-paths-raw))

(defn solve-1
  {:test (fn []
           (is= (solve-1 test-input) 5))}
  [input]
  (count (get-paths (create-state input) :you :out)))

(defn solve-2
  {:test (fn []
           (is= (solve-2 test-input-2) 2))}
  [input]
  (let [state (create-state input)
        number-of-path-between-dac-and-fft (number-of-paths state :dac :fft)
        first-intermediate-device (if (zero? number-of-path-between-dac-and-fft) :fft :dac)
        second-intermediate-device (if (= first-intermediate-device :dac) :fft :dac)]
    (* (number-of-paths state :svr first-intermediate-device)
       (if-not (zero? number-of-path-between-dac-and-fft)
         number-of-path-between-dac-and-fft
         (number-of-paths state :fft :dac))
       (number-of-paths state second-intermediate-device :out))))

(comment
  (time (solve-1 input))
  ; "Elapsed time: 3.784959 msecs"
  ; => 431
  (time (solve-2 input))
  ; "Elapsed time: 5.238917 msecs"
  ; => 358458157650450
  )
