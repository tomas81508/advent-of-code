(ns advent-of-code.dec-2021.day-12
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains?]]))

(def puzzle-input ["kc-qy" "qy-FN" "kc-ZP" "end-FN" "li-ZP" "yc-start" "end-qy" "yc-ZP" "wx-ZP" "qy-li" "yc-li" "yc-wx" "kc-FN" "FN-li" "li-wx" "kc-wx" "ZP-start" "li-kc" "qy-nv" "ZP-qy" "nv-xr" "wx-start" "end-nv" "kc-nv" "nv-XQ"])

(def simple-test-input ["start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end"])

{:start #{:A :b}
 :A     #{:b :c :end}
 :b     #{:A :d :end}
 :c     #{:A}
 :d     #{:b}}

(defn create-state
  {:test (fn []
           (is= (create-state simple-test-input)
                {:start #{:A :b}
                 :A     #{:b :c :end}
                 :b     #{:A :d :end}
                 :c     #{:A}
                 :d     #{:b}}))}
  [input]
  (->> input
       (reduce (fn [a v]
                 (let [[_ v1 v2] (re-matches #"(.*)-(.*)" v)
                       v1 (keyword v1)
                       v2 (keyword v2)]
                   (cond (= v1 :start) (update a v1 (fn [vs] (set (conj vs v2))))
                         (= v1 :end) (update a v2 (fn [vs] (set (conj vs v1))))
                         (= v2 :start) (update a v2 (fn [vs] (set (conj vs v1))))
                         (= v2 :end) (update a v1 (fn [vs] (set (conj vs v2))))
                         :else (-> a
                                   (update v1 (fn [vs] (set (conj vs v2))))
                                   (update v2 (fn [vs] (set (conj vs v1))))))))
               {})))

(def simple-test-state (create-state simple-test-input))
(def puzzle-state (create-state puzzle-input))

(defn small-cave?
  {:test (fn []
           (is (small-cave? :a))
           (is-not (small-cave? :A))
           (is-not (small-cave? :start))
           (is-not (small-cave? :end)))}
  [cave]
  (and (not (contains? #{:start :end} cave))
       (let [cave-id-string (name cave)]
         (= cave-id-string (clojure.string/lower-case cave-id-string)))))

(defn walk
  {:test (fn []
           (is= (walk simple-test-state {:active #{[:start]}
                                         :paths  #{}})
                {:active #{[:start :A]
                           [:start :b]}
                 :paths  #{}})
           (is= (walk simple-test-state {:active #{[:start :A]
                                                   [:start :b]}
                                         :paths  #{}})
                {:active #{[:start :A :b] [:start :A :c]
                           [:start :b :A] [:start :b :d]}
                 :paths  #{[:start :A :end] [:start :b :end]}}))}
  [state {active :active paths :paths}]
  (reduce (fn [a active-path]
            (let [last-cave (last active-path)]
              (->> (get state last-cave)
                   (reduce (fn [a c]
                             (cond (= c :end)
                                   (update a :paths (fn [paths] (conj paths (conj active-path c))))

                                   (small-cave? c)
                                   (if (seq-contains? active-path c)
                                     a
                                     (update a :active (fn [active] (set (conj active (conj active-path c))))))

                                   :else
                                   (update a :active (fn [active] (set (conj active (conj active-path c)))))))
                           a))))
          {:paths paths}
          active))


(defn solver-a
  {:test (fn []
           (is= (solver-a simple-test-state)
                #{[:start :b :A :c :A :end]
                  [:start :b :end]
                  [:start :A :c :A :b :end]
                  [:start :A :b :end]
                  [:start :b :A :end]
                  [:start :A :b :A :end]
                  [:start :A :c :A :b :A :end]
                  [:start :A :c :A :end]
                  [:start :A :end]
                  [:start :A :b :A :c :A :end]}))}
  [state]
  (loop [paths-data {:active #{[:start]}
                     :paths  #{}}]
    (if (empty? (:active paths-data))
      (:paths paths-data)
      (recur (walk state paths-data)))))

(comment
  (time (count (solver-a puzzle-state)))
  ; "Elapsed time: 72.497045 msecs"
  5874
  )


(defn walk-2
  {:test (fn []
           (is= (walk-2 simple-test-state {:active #{[:start]}
                                           :paths  #{}})
                {:active #{[:start :A]
                           [:start :b]}
                 :paths  #{}})
           (is= (walk-2 simple-test-state {:active #{[:start :A]
                                                     [:start :b]}
                                           :paths  #{}})
                {:active #{[:start :A :b] [:start :A :c]
                           [:start :b :A] [:start :b :d]}
                 :paths  #{[:start :A :end] [:start :b :end]}}))}
  [state {active :active paths :paths}]
  (reduce (fn [a active-path]
            (let [last-cave (last active-path)]
              (->> (get state last-cave)
                   (reduce (fn [a c]
                             (cond (= c :end)
                                   (update a :paths (fn [paths] (conj paths (conj active-path c))))

                                   (small-cave? c)
                                   (if (and (->> active-path
                                                 (filter small-cave?)
                                                 (frequencies)
                                                 (vals)
                                                 (some (fn [v] (>= v 2))))
                                            (seq-contains? active-path c))
                                     a
                                     (update a :active (fn [active] (set (conj active (conj active-path c))))))

                                   :else
                                   (update a :active (fn [active] (set (conj active (conj active-path c)))))))
                           a))))
          {:paths paths}
          active))
(clojure.string/lower-case (name :k))

(defn solver-b
  {:test (fn []
           (is= (count (solver-b simple-test-state))
                36))}
  [state]
  (loop [paths-data {:active #{[:start]}
                     :paths  #{}}]
    (if (empty? (:active paths-data))
      (:paths paths-data)
      (recur (walk-2 state paths-data)))))


(comment
  (time (count (solver-b puzzle-state)))
  ; "Elapsed time: 5493.139713 msecs"
  153592
  )


