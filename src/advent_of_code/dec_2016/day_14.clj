(ns advent-of-code.dec-2016.day-14
  (:require [ysera.test :refer [deftest is= is is-not]]
            [digest :refer [md5]]))


(def puzzle-input "ihaygndm")

(def test-input "abc")

(defn apply-md5-n-times
  {:test (fn []
           (is= (apply-md5-n-times "abc0" 2017)
                "a107ff634856bb300138cac6568c0f24"))}
  [text n]
  (loop [i 1
         text (md5 text)]
    (if (= i n)
      text
      (recur (inc i) (md5 text)))))

(def apply-md5-n-times-memoized (memoize apply-md5-n-times))

(defn n-of-the-same-character?
  {:test (fn []
           (is (n-of-the-same-character? (md5 "abc18") 3))
           (is-not (n-of-the-same-character? (md5 "abc0") 3))
           (is (n-of-the-same-character? (md5 "abc816") 5)))}
  [text n]
  (loop [[f & r] text
         current-character nil
         occurrences 0]
    (cond (nil? f)
          false

          (= f current-character)
          (if (= occurrences (dec n))
            current-character
            (recur r current-character (inc occurrences)))

          :else
          (recur r f 1))))

(def n-of-the-same-character? (memoize n-of-the-same-character?))

(defn get-key-index
  {:test (fn []
           (is= (get-key-index 0 "abc" 1)
                39)
           (is= (get-key-index 0 "abc" 2017)
                10))}
  [index salt stretching]
  (loop [index index
         three-of-the-same nil
         sub-index index]
    (if three-of-the-same
      (if-not (< sub-index (+ index 1000))
        (recur (inc index) nil (inc index))
        (let [result (n-of-the-same-character? (apply-md5-n-times-memoized (str salt sub-index) stretching) 5)]
          (if (= result three-of-the-same)
            index
            (recur index three-of-the-same (inc sub-index)))))

      (if-let [result (n-of-the-same-character? (apply-md5-n-times (str salt index) stretching) 3)]
        (recur index result (inc sub-index))
        (recur (inc index) nil (inc sub-index))))))

(defn get-n-key-index
  {:test (fn []
           (is= (get-n-key-index "abc" 2 1)
                92))}
  [salt n stretching]
  (loop [index 0
         number-of-keys 0]
    (println index)
    (if (= number-of-keys n)
      index
      (recur (get-key-index (inc index) salt stretching)
             (inc number-of-keys)))))

(comment
  (time (get-n-key-index "abc" 64 1))
  ; "Elapsed time: 4390.179192 msecs"
  ; => 22728

  ; puzzle-a
  (time (get-n-key-index puzzle-input 64 1))
  ; "Elapsed time: 1368.977702 msecs"
  ; => 15035

  ; puzzle-b
  (time (get-n-key-index puzzle-input 64 2017))
  ; "Elapsed time: 100076.34335 msecs"
  ; => 19968
  )
