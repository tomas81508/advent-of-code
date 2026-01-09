(ns advent-of-code.dec-2018.day-22
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.core]
            [clojure.edn :as edn]
            [clojure.set :refer [union]]
            [clojure.string :as string]))

(def input {:depth  8787
            :target [10 725]})

(def test-input {:depth  510
                 :target [10 10]})

(defn get-erosion-level-raw
  {:test (fn []
           (is= (get-erosion-level-raw test-input 0) 510)
           (is= (get-erosion-level-raw test-input 16807) 17317)
           (is= (get-erosion-level-raw test-input 48271) 8415)
           (is= (get-erosion-level-raw test-input 145722555) 1805))}
  [input geologic-index]
  (mod (+ geologic-index (:depth input)) 20183))

(def get-erosion-level (memoize get-erosion-level-raw))

(declare get-geologic-index)
(defn get-geologic-index-raw
  {:test (fn []
           (is= (get-geologic-index test-input [0 0]) 0)
           (is= (get-geologic-index test-input [1 0]) 16807)
           (is= (get-geologic-index test-input [0 1]) 48271)
           (is= (get-geologic-index test-input [1 1]) 145722555)
           (is= (get-geologic-index test-input [10 10]) 0))}
  [input [x y :as region]]
  (cond (or (= region [0 0]) (= region (:target input))) 0
        (zero? x) (* y 48271)
        (zero? y) (* x 16807)
        :else
        (* (get-erosion-level input (get-geologic-index input [(dec x) y]))
           (get-erosion-level input (get-geologic-index input [x (dec y)])))))

(def get-geologic-index (memoize get-geologic-index-raw))

(defn get-type
  {:test (fn []
           (is= (get-type 510) :rocky)
           (is= (get-type 17317) :wet)
           (is= (get-type 1805) :narrow))}
  [erosion-level]
  (case (mod erosion-level 3)
    0 :rocky
    1 :wet
    2 :narrow))

(declare get-type-by-region)
(defn get-type-by-region-raw
  {:test (fn []
           (is= (get-type-by-region test-input [1 1]) :narrow)
           (is= (get-type-by-region test-input [2 2]) :wet)
           (is= (get-type-by-region test-input [3 3]) :rocky))}
  [input region]
  (get-type (get-erosion-level input (get-geologic-index input region))))
(def get-type-by-region (memoize get-type-by-region-raw))

(defn get-risk-level
  {:test (fn []
           (is= (get-risk-level test-input) 114))}
  [input]
  (->> (for [x (range (inc (get-in input [:target 0])))
             y (range (inc (get-in input [:target 1])))]
         [x y])
       (map (fn [region] (get-type-by-region input region)))
       (map (fn [type] (case type :rocky 0 :wet 1 :narrow 2)))
       (reduce + 0)))

(comment
  (time (get-risk-level input))
  ; "Elapsed time: 12.652584 msecs"
  ; => 8090
  )

(defn compare-fn
  {:test (fn []
           (is (neg? (compare-fn {:minutes 2 :region [0 0] :equipment :neither}
                                 {:minutes 2 :region [0 0] :equipment :torch}))))}
  [k1 k2]
  (let [minutes (compare (:minutes k1) (:minutes k2))]
    (if-not (zero? minutes)
      minutes
      (let [region (compare (:region k1) (:region k2))]
        (if-not (zero? region)
          region
          (compare (:equipment k1) (:equipment k2)))))))

(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn inside?
  {:test (fn []
           (is (inside? [2 0]))
           (is (inside? [2 3]))
           (is-not (inside? [-2 0]))
           (is-not (inside? [1 -2])))}
  [c]
  (every? (fn [c] (>= c 0)) c))

(defn allowed-equipment?
  {:test (fn []
           (is (allowed-equipment? :rocky :climbing-gear))
           (is (allowed-equipment? :rocky :torch))
           (is-not (allowed-equipment? :rocky :neither))
           (is (allowed-equipment? :wet :climbing-gear))
           (is (allowed-equipment? :wet :neither))
           (is-not (allowed-equipment? :wet :torch))
           (is (allowed-equipment? :narrow :torch))
           (is (allowed-equipment? :narrow :neither))
           (is-not (allowed-equipment? :narrow :climbing-gear)))}
  [type equipment]
  (case type
    :rocky (or (= equipment :climbing-gear) (= equipment :torch))
    :wet (or (= equipment :climbing-gear) (= equipment :neither))
    :narrow (or (= equipment :torch) (= equipment :neither))))

(defn deja-vu?
  {:test (fn []
           (is-not (deja-vu? [0 0] :torch 0 {}))
           (is-not (deja-vu? [0 0] :torch 1 {{:region [0 0] :equipment :torch} 3}))
           (is (deja-vu? [0 0] :torch 2 {{:region [0 0] :equipment :torch} 1})))}
  [region equipment minutes visited]
  (let [visited-region-minutes (get visited {:region region :equipment equipment})]
    (not (or (nil? visited-region-minutes)
             (< (inc minutes) visited-region-minutes)))))

(def initial-state {:visited {{:region [0 0] :equipment :torch} 0}
                    :active  (sorted-set-by compare-fn
                                            {:minutes 0 :region [0 0] :equipment :torch})})

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-input initial-state)
                {:visited {{:region [0 0] :equipment :torch}         0
                           {:region [0 1] :equipment :torch}         1
                           {:region [0 0] :equipment :climbing-gear} 7}
                 :active  #{{:minutes 1 :region [0 1] :equipment :torch}
                            {:minutes 7 :region [0 0] :equipment :climbing-gear}}})
           (is= (walk-a-step test-input
                             {:visited {{:region [0 0] :equipment :torch}         0
                                        {:region [0 1] :equipment :torch}         1
                                        {:region [0 0] :equipment :climbing-gear} 7}
                              :active  (sorted-set-by compare-fn
                                                      {:minutes 1 :region [0 1] :equipment :torch}
                                                      {:minutes 7 :region [0 0] :equipment :climbing-gear})})
                {:visited {{:region [0 0], :equipment :torch}         0,
                           {:region [0 1], :equipment :torch}         1,
                           {:region [0 0], :equipment :climbing-gear} 7,
                           {:region [1 1], :equipment :torch}         2,
                           {:region [0 2], :equipment :torch}         2,
                           {:region [0 1], :equipment :climbing-gear} 8},
                 :active  #{{:minutes 2, :region [0 2], :equipment :torch}
                            {:minutes 2, :region [1 1], :equipment :torch}
                            {:minutes 7, :region [0 0], :equipment :climbing-gear}
                            {:minutes 8, :region [0 1], :equipment :climbing-gear}}}))}
  [input {visited :visited active :active :as state}]
  (let [current (first active)
        minutes (:minutes current)
        region (:region current)
        equipment (:equipment current)
        type (get-type-by-region input region)
        new-currents-1 (->> directions
                            (keep (fn [d]
                                    (let [new-region (mapv + d region)]
                                      (when (inside? new-region)
                                        (let [new-region-type (get-type-by-region input new-region)
                                              new-minutes (inc minutes)]
                                          (when (and (allowed-equipment? new-region-type equipment)
                                                     (not (deja-vu? new-region equipment new-minutes visited)))
                                            {:minutes   new-minutes
                                             :region    new-region
                                             :equipment equipment})))))))
        new-current-2 (->> [:climbing-gear :neither :torch]
                           (keep (fn [e] (when (and (not= e equipment)
                                                    (allowed-equipment? type e)
                                                    (not (deja-vu? region e (+ minutes 7) visited)))
                                           {:minutes   (+ minutes 7)
                                            :region    region
                                            :equipment e}))))
        new-currents (concat new-currents-1 new-current-2)]
    (-> state
        (update :visited (fn [a] (reduce (fn [a nc]
                                           (assoc a (select-keys nc [:region :equipment]) (:minutes nc)))
                                         a
                                         new-currents)))
        (update :active (fn [active]
                          (as-> active $
                                (disj $ current)
                                (apply conj $ new-currents)))))))

(defn walk-to-target
  {:test (fn []
           (is= (walk-to-target test-input initial-state)
                45))}
  [input state]
  (loop [state state]
    (let [minutes (get (:visited state) {:region (:target input) :equipment :torch})]
      (if (and minutes
               (> (:minutes (first (:active state))) minutes))
        minutes
        (recur (walk-a-step input state))))))

(comment
  (time (walk-to-target input initial-state))
  ; too high 997
  )


















