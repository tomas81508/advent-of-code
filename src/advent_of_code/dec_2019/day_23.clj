(ns advent-of-code.dec-2019.day-23
  (:require [advent-of-code.test :refer [is=]]
            [advent-of-code.dec-2019.day-09 :as day-09]))

; With Daniel Gullberg

(def input (slurp "src/advent_of_code/dec_2019/day_23.txt"))

(def program (day-09/parse-program input))

(defn get-network-packets
  [computers]
  (->> computers
       (map :outputs)
       (remove empty?)
       (flatten)
       (partition 3)))

(defn send-packets
  [computers network-queue network-packets]
  (let [network-queue (reduce (fn [a [address X Y]]
                                (update a address (fnil conj []) [X Y]))
                              network-queue
                              network-packets)]
    [(->> computers
          (reduce-kv (fn [a address computer]
                       (assoc a address (day-09/run (assoc computer :outputs []) (first (get network-queue address [[-1]])))))
                     []))
     (reduce (fn [a address]
               (update a address (fn [queue]
                                   (into [] (rest queue)))))
             network-queue
             (keys network-queue))]))

(defn part-1
  [program]
  (loop [computers (->> (range 50)
                        (mapv (fn [address]
                                (day-09/run program [address -1]))))
         network-queue {}]
    (let [network-packets (get-network-packets computers)]
      (if-let [Y (some (fn [[address _ Y]]
                         (when (= address 255)
                           Y))
                       network-packets)]
        Y
        (let [[computers network-queue] (send-packets computers network-queue network-packets)]
          (recur computers network-queue))))))

(defn idle?
  [network-queue network-packets]
  (->> (conj (vals network-queue) network-packets)
       (remove empty?)
       (empty?)))

(defn part-2
  [program]
  (loop [[computers network-queue] [(->> (range 50)
                                         (mapv (fn [address]
                                                 (day-09/run program [address -1]))))
                                    {}]
         nat nil
         last-delivered-nat nil]
    (let [network-packets (get-network-packets computers)
          new-nat (some (fn [[address X Y]]
                          (when (= address 255)
                            [X Y]))
                        network-packets)]
      (cond
        (idle? network-queue network-packets)
        (if (and last-delivered-nat (= (second last-delivered-nat) (second nat)))
          (second nat)
          (recur (send-packets computers network-queue [[0 (first nat) (second nat)]]) nat nat))

        new-nat (recur (send-packets computers network-queue (remove (fn [[address _ _]]
                                                                       (= address 255))
                                                                     network-packets))
                       new-nat
                       last-delivered-nat)
        :else
        (recur (send-packets computers network-queue network-packets) nat last-delivered-nat)))))

(comment
  (time (part-1 program))
  ;; "Elapsed time: 28.035667 msecs"
  ;=> 19937

  (time (part-2 program))
  ;; "Elapsed time: 231.810333 msecs"
  ;=> 13758
  )
