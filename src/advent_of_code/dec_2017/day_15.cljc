(ns advent-of-code.dec-2017.day-15)

(def generator-A-start-value 783)
(def generator-B-start-value 325)

(def generator-A-test-start-value 65)
(def generator-B-test-start-value 8921)

(def generator-A-multiply-value 16807)
(def generator-B-multiply-value 48271)

(def dividing-number 2147483647)

(defn generate-values-for-A
  [value]
  (let [next-value (rem (* value generator-A-multiply-value)
                        dividing-number)]
    (cons next-value (lazy-seq (generate-values-for-A next-value)))))

(defn generate-values-for-B
  [value]
  (let [next-value (rem (* value generator-B-multiply-value)
                        dividing-number)]
    (cons next-value (lazy-seq (generate-values-for-B next-value)))))

(def A-test-values (generate-values-for-A generator-A-test-start-value))
(def B-test-values (generate-values-for-B generator-B-test-start-value))

(def AB-test-values (map (fn [a b] [a b]) A-test-values B-test-values))

(def A-values (generate-values-for-A generator-A-start-value))
(def B-values (generate-values-for-B generator-B-start-value))

(def AB-values (map (fn [a b] [a b]) A-values B-values))

(comment
  (Math/pow 2 16)
  )

(defn check-numbers
  [a b]
  (= (rem a 65536) (rem b 65536)))

(comment
  (->> AB-values
       (take 40000000)
       (reduce (fn [acc [av bv]] (if (check-numbers av bv) (inc acc) acc))
               0))
  )

; part 2
(defn generate-values-for-A-2
  [value]
  (let [next-value (rem (* value generator-A-multiply-value)
                        dividing-number)]
    (if-not (zero? (rem next-value 4))
      (generate-values-for-A-2 next-value)
      (cons next-value (lazy-seq (generate-values-for-A-2 next-value))))))

(defn generate-values-for-B-2
  [value]
  (let [next-value (rem (* value generator-B-multiply-value)
                        dividing-number)]
    (if-not (zero? (rem next-value 8))
      (generate-values-for-B-2 next-value)
      (cons next-value (lazy-seq (generate-values-for-B-2 next-value))))))

(def A-test-values-2 (generate-values-for-A-2 generator-A-test-start-value))
(def B-test-values-2 (generate-values-for-B-2 generator-B-test-start-value))

(def AB-test-values-2 (map (fn [a b] [a b]) A-test-values-2 B-test-values-2))

(def A-values-2 (generate-values-for-A-2 generator-A-start-value))
(def B-values-2 (generate-values-for-B-2 generator-B-start-value))

(def AB-values-2 (map (fn [a b] [a b]) A-values-2 B-values-2))

(comment
  (->> AB-values-2
       (take 5000000)
       (reduce (fn [acc [av bv]] (if (check-numbers av bv) (inc acc) acc))
               0))
  )
