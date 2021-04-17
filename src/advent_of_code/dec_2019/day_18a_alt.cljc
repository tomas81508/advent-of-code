(ns advent-of-code.dec-2019.day-18a-alt
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.string :refer [lower-case]]
            [clojure.set :refer [subset?]]))

(defn create-state
  {:test (fn []
           (is= (create-state "#########"
                              "#b.A.@.a#"
                              "#########")
                {:start        [5 1]
                 :walls        #{[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] [8 0]
                                 [0 1] [8 1]
                                 [0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2] [7 2] [8 2]}
                 :open-passage #{[2 1] [4 1] [6 1] [5 1]}
                 :keys         {[1 1] :b
                                [7 1] :a}
                 :doors        {[3 1] :a}}))}
  [& strings]
  (->> strings
       (map-indexed (fn [y row]
                      (->> row
                           (map-indexed (fn [x l]
                                          {:position [x y]
                                           :letter   l})))))
       (flatten)
       (reduce (fn [a {l :letter position :position}]
                 (cond (= l \@)
                       (-> a
                           (update :open-passage conj position)
                           (assoc :start position))

                       (= l \#)
                       (update a :walls conj position)

                       (= l \.)
                       (update a :open-passage conj position)

                       (re-matches #"[a-z]" (str l))
                       (update a :keys assoc position (keyword (str l)))

                       (re-matches #"[A-Z]" (str l))
                       (update a :doors assoc position (keyword (lower-case (str l))))))
               {:walls        #{}
                :open-passage #{}
                :keys         {}
                :doors        {}})))

(defn wall?
  {:test (fn []
           (is (-> (create-state "#########"
                                 "#b.A.@.a#"
                                 "#########")
                   (wall? [0 0])))
           (is-not (-> (create-state "#########"
                                     "#b.A.@.a#"
                                     "#########")
                       (wall? [1 1]))))}
  [state position]
  (contains? (:walls state) position))

(defn open-passage?
  {:test (fn []
           (is (-> (create-state "#########"
                                 "#b.A.@.a#"
                                 "#########")
                   (open-passage? [2 1])))
           (is-not (-> (create-state "#########"
                                     "#b.A.@.a#"
                                     "#########")
                       (open-passage? [1 1]))))}
  [state position]
  (contains? (:open-passage state) position))

(defn key?
  {:test (fn []
           (is (-> (create-state "#########"
                                 "#b.A.@.a#"
                                 "#########")
                   (key? [1 1])))
           (is-not (-> (create-state "#########"
                                     "#b.A.@.a#"
                                     "#########")
                       (key? [0 0]))))}
  [state position]
  (contains? (:keys state) position))

(defn door?
  {:test (fn []
           (is (-> (create-state "#########"
                                 "#b.A.@.a#"
                                 "#########")
                   (door? [3 1])))
           (is-not (-> (create-state "#########"
                                     "#b.A.@.a#"
                                     "#########")
                       (door? [0 0]))))}
  [state position]
  (contains? (:doors state) position))

(defn get-keys-iteration-step
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (get-keys-iteration-step {[5 1] {:doors #{} :movement 0}} 0))
                {[5 1] {:doors #{} :movement 0}
                 [4 1] {:doors #{} :movement 1}
                 [6 1] {:doors #{} :movement 1}})
           (is= (let [state (create-state "#########"
                                          "#b.A.@.a#"
                                          "#########")]
                  (as-> (get-keys-iteration-step state {[5 1] {:doors #{} :movement 0}} 0) $
                        (get-keys-iteration-step state $ 1)))
                {[5 1] {:doors #{} :movement 0}
                 [4 1] {:doors #{} :movement 1}
                 [6 1] {:doors #{} :movement 1}
                 [3 1] {:doors #{:a} :movement 2}
                 [7 1] {:doors #{} :movement 2 :at-key :a}})
           (is= (let [state (create-state "#########"
                                          "#b.A.@.a#"
                                          "#########")]
                  (as-> (get-keys-iteration-step state {[5 1] {:doors #{} :movement 0}} 0) $
                        (get-keys-iteration-step state $ 1)
                        (get-keys-iteration-step state $ 2)))
                {[5 1] {:doors #{} :movement 0}
                 [4 1] {:doors #{} :movement 1}
                 [6 1] {:doors #{} :movement 1}
                 [3 1] {:doors #{:a} :movement 2}
                 [7 1] {:doors #{} :movement 2 :at-key :a}
                 [2 1] {:doors #{:a} :movement 3}})
           (is= (let [state (create-state "#########"
                                          "#b.A.@.a#"
                                          "#########")]
                  (as-> (get-keys-iteration-step state {[5 1] {:doors #{} :movement 0}} 0) $
                        (get-keys-iteration-step state $ 1)
                        (get-keys-iteration-step state $ 2)
                        (get-keys-iteration-step state $ 3)))
                {[5 1] {:doors #{} :movement 0}
                 [4 1] {:doors #{} :movement 1}
                 [6 1] {:doors #{} :movement 1}
                 [3 1] {:doors #{:a} :movement 2}
                 [7 1] {:doors #{} :movement 2 :at-key :a}
                 [2 1] {:doors #{:a} :movement 3}
                 [1 1] {:doors #{:a} :movement 4 :at-key :b}}))}
  [state visits movement]
  (let [current-positions (->> (seq visits)
                               (filter (fn [[_ {m :movement at-key :at-key}]]
                                         (and (= m movement) (not at-key)))))]
    (reduce (fn [visits [current-position visit-info]]
              (let [new-positions (->> [[-1 0] [1 0] [0 -1] [0 1]]
                                       (map (fn [d] (mapv + d current-position)))
                                       (remove (fn [p] (wall? state p))))]
                (reduce (fn [visits new-position]
                          (cond (contains? visits new-position)
                                visits

                                (door? state new-position)
                                (assoc visits new-position (-> visit-info
                                                               (update :movement inc)
                                                               (update :doors conj (get-in state [:doors new-position]))))

                                (key? state new-position)
                                (assoc visits new-position (-> visit-info
                                                               (update :movement inc)
                                                               (assoc :at-key (get-in state [:keys new-position]))))

                                :else
                                (assoc visits new-position (update visit-info :movement inc))))
                        visits
                        new-positions)))
            visits
            current-positions)))


(defn get-closest-keys
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (get-closest-keys [5 1]))
                {:a {:distance 2 :required-keys #{}}
                 :b {:distance 4 :required-keys #{:a}}})
           (is= (-> (create-state "########################"
                                  "#f.D.E.e.C.b.A.@.a.B.c.#"
                                  "######################.#"
                                  "#d.....................#"
                                  "########################")
                    (get-closest-keys [15 1]))
                {:a {:distance 2 :required-keys #{}}
                 :b {:distance 4 :required-keys #{:a}}}))}
  [state position]
  (loop [movement 0
         visits {position {:doors #{} :movement 0}}]
    (let [new-visits (get-keys-iteration-step state visits movement)]
      (if (not= new-visits visits)
        (recur (inc movement)
               new-visits)
        (->> (vals visits)
             (filter :at-key)
             (reduce (fn [result {doors :doors movement :movement at-key :at-key}]
                       (assoc result at-key {:distance      movement
                                             :required-keys doors}))
                     {}))))))

(defn create-graph
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (create-graph))
                {:start {:a {:distance 2 :required-keys #{}}
                         :b {:distance 4 :required-keys #{:a}}}
                 :a     {:b {:distance 6 :required-keys #{:a}}}
                 :b     {:a {:distance 6 :required-keys #{:a}}}})
           (is= (-> (create-state "########################"
                                  "#f.D.E.e.C.b.A.@.a.B.c.#"
                                  "######################.#"
                                  "#d.....................#"
                                  "########################")
                    (create-graph))
                {:start {:a {:distance 2 :required-keys #{}}
                         :b {:distance 4 :required-keys #{:a}}}
                 :f     {:e {:distance 6 :required-keys #{:e :d}}}
                 :e     {:f {:distance 6 :required-keys #{:e :d}}
                         :b {:distance 4 :required-keys #{:c}}}
                 :b     {:e {:distance 4 :required-keys #{:c}}
                         :a {:distance 6 :required-keys #{:a}}}
                 :a     {:c {:distance 4 :required-keys #{:b}}
                         :b {:distance 6 :required-keys #{:a}}}
                 :c     {:a {:distance 4 :required-keys #{:b}}
                         :d {:distance 24 :required-keys #{}}}
                 :d     {:c {:distance 24 :required-keys #{}}}}))}
  [state]
  (let [->position (assoc (clojure.set/map-invert (:keys state))
                     :start (:start state))]
    (reduce (fn [a v]
              (assoc a v (get-closest-keys state (->position v))))
            {}
            (conj (vals (:keys state)) :start))))

(defn collect-all-keys-iteration-step
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (create-graph)
                    (collect-all-keys-iteration-step {:queued [{:position       :start
                                                                :collected-keys #{}
                                                                :movement       0}]}))
                {:queued [{:position       :a
                           :collected-keys #{:a}
                           :movement       2}]
                 :start  {#{} 0}})
           (is= (let [graph (-> (create-state "#########"
                                              "#b.A.@.a#"
                                              "#########")
                                (create-graph))]
                  (->> {:queued [{:position       :start
                                  :collected-keys #{}
                                  :movement       0}]}
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)))
                {:queued [{:position :b :collected-keys #{:b :a} :movement 8}]
                 :start  {#{} 0}
                 :a      {#{:a} 2}})
           (is= (let [graph (-> (create-state "#########"
                                              "#b.A.@.a#"
                                              "#########")
                                (create-graph))]
                  (->> {:queued [{:position       :start
                                  :collected-keys #{}
                                  :movement       0}]}
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)))
                {:queued [{:position       :a
                           :collected-keys #{:b :a}
                           :movement       14}]
                 :start  {#{} 0}
                 :a      {#{:a} 2}
                 :b      {#{:b :a} 8}})
           (is= (let [graph (-> (create-state "#########"
                                              "#b.A.@.a#"
                                              "#########")
                                (create-graph))]
                  (->> {:queued [{:position       :start
                                  :collected-keys #{}
                                  :movement       0}]}
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)))
                {:queued []
                 :start  {#{} 0}
                 :a      {#{:a}    2
                          #{:b :a} 14}
                 :b      {#{:b :a} 8}}))}
  [graph visits]
  (reduce (fn [visits {position :position collected-keys :collected-keys movement :movement}]
            (let [old-visit-movement (get-in visits [position collected-keys])]
              (if (and old-visit-movement (< old-visit-movement movement))
                visits
                (let [new-keys-to-queue (->> (get graph position)
                                             (reduce-kv (fn [queued k {distance      :distance
                                                                       required-keys :required-keys}]
                                                          (if-not (subset? required-keys collected-keys)
                                                            queued
                                                            (conj queued {:position       k
                                                                          :collected-keys (conj collected-keys k)
                                                                          :movement       (+ movement distance)})))
                                                        []))]
                  (-> (assoc-in visits [position collected-keys] movement)
                      (update :queued (fn [queued] (reduce conj queued new-keys-to-queue))))))))
          (assoc visits :queued [])
          (:queued visits)))

(defn find-min-path-to-all-keys
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (create-graph)
                    (find-min-path-to-all-keys {:queued [{:position       :b
                                                          :collected-keys #{:b :a}
                                                          :movement       8}]
                                                :start  {#{} 0}
                                                :a      {#{:a} 2}}))
                nil)
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (create-graph)
                    (find-min-path-to-all-keys {:queued []
                                                :start  {#{} 0}
                                                :a      {#{:a}    2
                                                         #{:b :a} 14}
                                                :b      {#{:b :a} 8}}))
                8))}
  [graph visits]
  (let [all-keys (->> (dissoc graph :start)
                      (keys)
                      (set))]
    (reduce (fn [result part-results]
              (let [movement (get part-results all-keys)]
                (cond (not result) movement
                      (not movement) result
                      (< movement result) movement
                      :else result)))
            nil
            (vals visits))))

(defn collect-all-keys
  {:test (fn []
           (is= (let [graph (-> (create-state "#########"
                                              "#b.A.@.a#"
                                              "#########")
                                (create-graph))]
                  (->> graph
                       (collect-all-keys)
                       (find-min-path-to-all-keys graph)))
                8))}
  [graph]
  (loop [result {:queued [{:position       :start
                           :collected-keys #{}
                           :movement       0}]}]
    (let [new-result (collect-all-keys-iteration-step graph result)]
      (if (= new-result result)
        result
        (recur new-result)))))

(defn solve-puzzle [& strings]
  (time (let [graph (-> (apply create-state strings)
                        (create-graph))]
          (->> (collect-all-keys graph)
               (find-min-path-to-all-keys graph)))))

(comment
  (solve-puzzle "########################"
                "#f.D.E.e.C.b.A.@.a.B.c.#"
                "######################.#"
                "#d.....................#"
                "########################")
  ; "Elapsed time: 2.12423 msecs"
  ; => 86
  )

(comment
  (solve-puzzle "#################"
                "#i.G..c...e..H.p#"
                "########.########"
                "#j.A..b...f..D.o#"
                "########@########"
                "#k.E..a...g..B.n#"
                "########.########"
                "#l.F..d...h..C.m#"
                "#################")

  ; "Elapsed time: 6904.013933 msecs"
  ; => 136
  )

(comment
  (solve-puzzle "########################"
                "#@..............ac.GI.b#"
                "###d#e#f################"
                "###A#B#C################"
                "###g#h#i################"
                "########################")
  ; "Elapsed time: 23.662885 msecs"
  ; => 81
  )

(comment
  (->> (slurp "src/advent_of_code/dec_2019/day_18.txt")
       (clojure.string/split-lines)
       (apply solve-puzzle))
  ; "Elapsed time: 22027.826651 msecs"
  ; => 3048
  )











