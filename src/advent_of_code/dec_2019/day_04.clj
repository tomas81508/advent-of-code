(ns advent-of-code.dec-2019.day-04
  (:require [ysera.test :refer [is is-not is= deftest]]))

(def puzzle-input {:min 156218
                   :max 652527})

(defn conditions-satisfied?
  {:test (fn []
           (is (conditions-satisfied? 222222))
           (is-not (conditions-satisfied? 223450))
           (is-not (conditions-satisfied? 234789)))}
  [number]
  (and (let [number-strings (clojure.string/split (str number) #"")]
         (and (= number-strings (sort number-strings))
              (loop [[f s & r] number-strings]
                (cond (not s) false
                      (= f s) true
                      :else (recur (cons s r))))))))

(deftest puzzle-a
         (is= (time (->> (range (:min puzzle-input) (inc (:max puzzle-input)))
                         (filter conditions-satisfied?)
                         (count)))
              ; "Elapsed time: 654.629057 msecs"
              1694))

(defn conditions-satisfied-2?
  {:test (fn []
           (is-not (conditions-satisfied-2? 222222))
           (is (conditions-satisfied-2? 223445))
           (is (conditions-satisfied-2? 222233))
           (is-not (conditions-satisfied-2? 223450))
           (is-not (conditions-satisfied-2? 234789)))}
  [number]
  (and (let [number-strings (clojure.string/split (str number) #"")]
         (and (= number-strings (sort number-strings))
              (-> (frequencies number-strings)
                  (vals)
                  (set)
                  (contains? 2))))))

(deftest puzzle-b
         (is= (time (->> (range (:min puzzle-input) (inc (:max puzzle-input)))
                         (filter conditions-satisfied-2?)
                         (count)))
              ; "Elapsed time: 658.763168 msecs"
              1148))




