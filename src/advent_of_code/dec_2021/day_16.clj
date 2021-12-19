(ns advent-of-code.dec-2021.day-16
  (:require [ysera.test :refer [is= deftest]]))

(def puzzle-input (slurp "src/advent_of_code/dec_2021/day_16_input.txt"))

(def hex->binary {\0 [0 0 0 0] \1 [0 0 0 1] \2 [0 0 1 0] \3 [0 0 1 1] \4 [0 1 0 0] \5 [0 1 0 1] \6 [0 1 1 0] \7 [0 1 1 1]
                  \8 [1 0 0 0] \9 [1 0 0 1] \A [1 0 1 0] \B [1 0 1 1] \C [1 1 0 0] \D [1 1 0 1] \E [1 1 1 0] \F [1 1 1 1]})

(defn get-binary
  {:test (fn []
           (is= (get-binary "D2FE28")
                [1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0]))}
  [hexadecimals]
  (->> hexadecimals
       (map hex->binary)
       (flatten)))

(def puzzle-binary (get-binary puzzle-input))

(defn get-value
  {:test (fn []
           (is= (get-value [1 1 0]) 6)
           (is= (get-value [1 0 0 1 0 1 1 0 1 0 0 1 1 1 0 1 1 1 0 1 1 0 0 0 1 0 0 1 0 0 0 0])
                2526926992))}
  [coll]
  (-> coll
      (clojure.string/join)
      (Long/parseLong 2)))

(defn extract-version
  {:test (fn []
           (is= (extract-version [1 1 0 1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0])
                [[1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0] 6]))}
  [stream]
  (let [version (get-value (take 3 stream))]
    [(drop 3 stream) version]))

(defn extract-type-id
  {:test (fn []
           (is= (extract-type-id [1 0 0 1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0])
                [[1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0] 4]))}
  [stream]
  {:pre [(not (empty? stream))]}
  (let [type-id (get-value (take 3 stream))]
    [(drop 3 stream) type-id]))

(defn extract-value
  {:test (fn []
           (is= (extract-value [1 0 1 1 1 1 1 1 1 0 0 0 1 0 1 0 0 0])
                [[0 0 0] 2021 15]))}
  [stream]
  (loop [stream stream
         result []
         size 0]
    (let [group (take 5 stream)
          more-exist (not (zero? (first group)))
          value (drop 1 group)
          stream (drop 5 stream)
          result (apply conj result value)
          size (+ size 5)]
      (if more-exist
        (recur stream result size)
        [stream (get-value result) size]))))

(defn extract-length-type-id
  {:test (fn []
           (is= (extract-length-type-id [0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0])
                [[0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0] 0]))}
  [stream]
  [(drop 1 stream) (first stream)])

(defn extract-binary-length
  {:test (fn []
           (is= (extract-binary-length [0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0])
                [[1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0] 27]))}
  [stream]
  [(drop 15 stream) (get-value (take 15 stream))])

(defn extract-number-of-sub-packets
  {:test (fn []
           (is= (extract-number-of-sub-packets [0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0])
                [[0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0] 3]))}
  [stream]
  [(drop 11 stream) (get-value (take 11 stream))])

(declare extract-packet)

(defn extract-binary-sub-packets
  {:test (fn []
           (is= (extract-binary-sub-packets [1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0] 27)
                [[0 0 0 0 0 0 0]
                 [{:version 6 :type-id 4 :value 10}
                  {:version 2 :type-id 4 :value 20}]
                 27])
           (is= (extract-binary-sub-packets [1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0] 27)
                [[]
                 [{:version 6 :type-id 4 :value 10}
                  {:version 2 :type-id 4 :value 20}]
                 27]))}
  [stream binary-length]
  (loop [stream stream
         result []
         total-size 0]
    (let [[stream version] (extract-version stream)
          [stream type-id] (extract-type-id stream)

          [stream result size]
          (if (= type-id 4)
            (let [[stream value size] (extract-value stream)]
              [stream
               (conj result {:version version :type-id type-id :value value})
               (+ size 6)])
            (let [[stream packet size] (extract-packet stream version type-id)]
              [stream (conj result packet) size]))

          total-size (+ total-size size)]
      (cond (> total-size binary-length)
            (/ 1 0)
            (= total-size binary-length)
            [stream result total-size]
            :else
            (recur stream result total-size)))))

(defn extract-sub-packets
  {:test (fn []
           (is= (extract-sub-packets [0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0] 3)
                [[0 0 0 0 0]
                 [{:version 2 :type-id 4 :value 1}
                  {:version 4 :type-id 4 :value 2}
                  {:version 1 :type-id 4 :value 3}]
                 33])
           )}
  [stream n]
  (loop [stream stream
         result []
         i 0
         total-size 0]
    (let [[stream version] (extract-version stream)
          [stream type-id] (extract-type-id stream)

          [stream result size]
          (if (= type-id 4)
            (let [[stream value size] (extract-value stream)]
              [stream
               (conj result {:version version :type-id type-id :value value})
               (+ size 6)])
            (let [[stream packet size] (extract-packet stream version type-id)]
              [stream (conj result packet) size]))

          i (inc i)
          total-size (+ size total-size)]

      (if (= i n)
        [stream result total-size]
        (recur stream result i total-size)))))

(defn extract-packet
  {:test (fn []
           (is= (extract-packet [0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0])
                [[0 0 0 0 0 0 0]
                 {:version 1
                  :type-id 6
                  :packets [{:version 6 :type-id 4 :value 10}
                            {:version 2 :type-id 4 :value 20}]}
                 49])
           (is= (extract-packet [1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 1 1 0 0 1 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 0 0 0 0 0])
                [[0 0 0 0 0]
                 {:version 7
                  :type-id 3
                  :packets [{:version 2 :type-id 4 :value 1}
                            {:version 4 :type-id 4 :value 2}
                            {:version 1 :type-id 4 :value 3}]}
                 51])
           (is= (extract-packet [1 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 1 0 1 0 0 0 1 1 1 1 0 0 0])
                [[0 0 0]
                 {:version 4
                  :type-id 2
                  :packets [{:version 1
                             :type-id 2
                             :packets [{:version 5
                                        :type-id 2
                                        :packets [{:version 6
                                                   :type-id 4
                                                   :value   15}]}]}]}
                 69])
           (is= (extract-packet (->> "620080001611562C8802118E34"
                                     (get-binary)))
                [[0 0]
                 {:version 3,
                  :type-id 0,
                  :packets [{:version 0 :type-id 0 :packets [{:version 0 :type-id 4 :value 10} {:version 5 :type-id 4 :value 11}]}
                            {:version 1 :type-id 0 :packets [{:version 0 :type-id 4 :value 12} {:version 3 :type-id 4 :value 13}]}]}
                 102])
           (is= (extract-packet (->> "C0015000016115A2E0802F182340"
                                     (get-binary)))
                [[0 0 0 0 0 0]
                 {:version 6
                  :type-id 0
                  :packets [{:version 0
                             :type-id 0
                             :packets [{:version 0 :type-id 4 :value 10}
                                       {:version 6 :type-id 4 :value 11}]}
                            {:version 4
                             :type-id 0
                             :packets [{:version 7 :type-id 4 :value 12}
                                       {:version 0 :type-id 4 :value 13}]}]}
                 106])
           )}
  ([stream] (extract-packet stream nil nil))
  ([stream version type-id]
   (let [[stream version] (if version [stream version] (extract-version stream))
         [stream type-id] (if type-id [stream type-id] (extract-type-id stream))
         [stream length-type-id] (extract-length-type-id stream)
         [stream sub-packets size]
         (if (zero? length-type-id)
           (let [[stream binary-length] (extract-binary-length stream)
                 [stream packets size] (extract-binary-sub-packets stream binary-length)]
             [stream packets (+ size 15)])
           (let [[stream number-of-sub-packets] (extract-number-of-sub-packets stream)
                 [stream packets size] (extract-sub-packets stream number-of-sub-packets)]
             [stream packets (+ size 11)]))
         size (+ size 7)]
     [stream
      {:version version
       :type-id type-id
       :packets sub-packets}
      size])))


(defn get-version-sum
  {:test (fn []
           (is= (->> "8A004A801A8002F478"
                     (get-binary)
                     (extract-packet)
                     (second)
                     (get-version-sum))
                16)
           (is= (->> "C0015000016115A2E0802F182340"
                     (get-binary)
                     (extract-packet)
                     (second)
                     (get-version-sum))
                23)
           (is= (->> "A0016C880162017C3686B18A3D4780"
                     (get-binary)
                     (extract-packet)
                     (second)
                     (get-version-sum))
                31)
           )}
  [packet]
  (apply +
         (:version packet)
         (->> (:packets packet)
              (map get-version-sum))))

(deftest puzzle-a
         (is= (time (->> puzzle-binary
                         (extract-packet)
                         (second)
                         (get-version-sum)))
              ; "Elapsed time: 2.981929 msecs"
              875)
         )

(defn calculate
  {:test (fn []
           (is= (calculate {:version 6, :type-id 0, :packets [{:version 6, :type-id 4, :value 1} {:version 2, :type-id 4, :value 2}]})
                3)
           (is= (->> "04005AC33890"
                     (get-binary)
                     (extract-packet)
                     (second)
                     (calculate))
                54))}
  [{type-id :type-id packets :packets value :value}]
  (condp = type-id
    0 (apply + (map calculate packets))
    1 (apply * (map calculate packets))
    2 (apply min (map calculate packets))
    3 (apply max (map calculate packets))
    4 value
    5 (if (apply > (map calculate packets)) 1 0)
    6 (if (apply < (map calculate packets)) 1 0)
    7 (if (apply = (map calculate packets)) 1 0)))

(deftest puzzle-b
         (is= (time (let [[_ state] (->> puzzle-binary
                                         (extract-packet))]
                      (calculate state)))
              ; "Elapsed time: 3.777419 msecs"
              1264857437203))
