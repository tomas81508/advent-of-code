(ns advent-of-code.dec-2019.day-18b
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.string :refer [lower-case]]
            [advent-of-code.dec-2019.day-18 :refer [door?
                                                    key?
                                                    open-passage?
                                                    wall?]]
            [clojure.set :refer [subset?]]))


(defn create-state
  {:test (fn []
           (is= (create-state "#######"
                              "#a.#Cd#"
                              "##@#@##"
                              "#######"
                              "##@#@##"
                              "#cB#Ab#"
                              "#######")
                {:walls        #{[4 3] [0 0] [1 0] [2 3] [0 6] [3 3] [5 4] [6 3] [0 5] [3 4] [3 0] [6 6] [5 3] [6 5] [5 2]
                                 [4 6] [1 4] [1 3] [6 4] [0 3] [6 1] [5 6] [3 6] [0 2] [2 0] [0 4] [3 1] [1 6] [2 6] [5 0]
                                 [6 2] [6 0] [1 2] [3 5] [3 2] [0 1] [4 0]}
                 :open-passage #{[2 2] [4 2] [2 4] [2 1] [4 4]}
                 :keys         {[1 1] :a [5 1] :d [1 5] :c [5 5] :b}
                 :doors        {[4 1] :c [2 5] :b [4 5] :a}
                 :start        {0 [2 2] 1 [4 2] 2 [2 4] 3 [4 4]}}))}
  [& strings]
  (as-> strings $
        (map-indexed (fn [y row]
                       (->> row
                            (map-indexed (fn [x l]
                                           {:position [x y]
                                            :letter   l}))))
                     $)
        (flatten $)
        (reduce (fn [a {l :letter position :position}]
                  (cond (= l \@)
                        (-> a
                            (update :open-passage conj position)
                            (assoc-in [:start (:counter a)] position)
                            (update :counter inc))

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
                 :doors        {}
                 :start        {}
                 :counter      0}
                $)
        (dissoc $ :counter)))

(defn get-keys-iteration-step
  {:test (fn []
           (is= (-> (create-state "#######"
                                  "#a.#Cd#"
                                  "##@#@##"
                                  "#######"
                                  "##@#@##"
                                  "#cB#Ab#"
                                  "#######")
                    (get-keys-iteration-step {[2 2] {:doors #{} :movement 0 :passing-keys #{}}} 0))
                {[2 2] {:doors #{} :passing-keys #{} :movement 0}
                 [2 1] {:doors #{} :passing-keys #{} :movement 1}})
           (is= (let [state (create-state "#####"
                                          "#aB##"
                                          "##b@#"
                                          "#####")]
                  (as-> (get-keys-iteration-step state
                                                 {[3 2] {:doors #{} :passing-keys #{} :movement 0}}
                                                 0) $
                        (get-keys-iteration-step state $ 1)))
                {[3 2] {:doors #{} :passing-keys #{} :movement 0}
                 [2 2] {:doors #{} :passing-keys #{} :movement 1 :at-key :b}
                 [2 1] {:doors #{:b} :passing-keys #{:b} :movement 2}}))}
  [state visits movement]
  (let [current-positions (->> (seq visits)
                               (filter (fn [[_ {m :movement}]]
                                         (= m movement))))]
    (reduce (fn [visits [current-position visit-info]]
              (let [new-positions (->> [[-1 0] [1 0] [0 -1] [0 1]]
                                       (map (fn [d] (mapv + d current-position)))
                                       (remove (fn [p] (wall? state p))))]
                (reduce (fn [visits new-position]
                          (if (contains? visits new-position)
                            visits
                            (let [new-visit-info (-> (if-not (:at-key visit-info)
                                                       visit-info
                                                       (update visit-info :passing-keys conj (:at-key visit-info)))
                                                     (dissoc :at-key)
                                                     (update :movement inc))]
                              (cond (door? state new-position)
                                    (assoc visits new-position (-> new-visit-info
                                                                   (update :doors conj (get-in state [:doors new-position]))))

                                    (key? state new-position)
                                    (assoc visits new-position (-> new-visit-info
                                                                   (assoc :at-key (get-in state [:keys new-position]))))

                                    :else
                                    (assoc visits new-position new-visit-info)))))
                        visits
                        new-positions)))
            visits
            current-positions)))

(defn get-keys-from-position
  {:test (fn []
           (is= (-> (create-state "#######"
                                  "#a.#Cd#"
                                  "##@#@##"
                                  "#######"
                                  "##@#@##"
                                  "#cB#Ab#"
                                  "#######")
                    (get-keys-from-position [2 2]))
                {:a {:distance 2 :passing-keys #{} :required-keys #{}}})
           (is= (-> (create-state "####"
                                  "##@#"
                                  "##B#"
                                  "#ce#"
                                  "####")
                    (get-keys-from-position [2 1]))
                {:c {:distance 3 :passing-keys #{:e} :required-keys #{:b}}
                 :e {:distance 2 :passing-keys #{} :required-keys #{:b}}}))}
  [state position]
  (loop [movement 0
         visits {position {:doors #{} :movement 0 :passing-keys #{}}}]
    (let [new-visits (get-keys-iteration-step state visits movement)]
      (if (not= new-visits visits)
        (recur (inc movement)
               new-visits)
        (->> (vals visits)
             (filter :at-key)
             (reduce (fn [result {doors :doors passing-keys :passing-keys movement :movement at-key :at-key}]
                       (assoc result at-key {:distance      movement
                                             :passing-keys  passing-keys
                                             :required-keys doors}))
                     {}))))))

(defn create-graph
  {:test (fn []
           (is= (-> (create-state "#######"
                                  "#a.#Cd#"
                                  "##@#@##"
                                  "#######"
                                  "##@#@##"
                                  "#cB#Ab#"
                                  "#######")
                    (create-graph))
                {0  {:a {:distance 2 :passing-keys #{} :required-keys #{}}}
                 1  {:d {:distance 2 :passing-keys #{} :required-keys #{:c}}}
                 2  {:c {:distance 2 :passing-keys #{} :required-keys #{:b}}}
                 3  {:b {:distance 2 :passing-keys #{} :required-keys #{:a}}}
                 :a {}
                 :b {}
                 :c {}
                 :d {}})
           (is= (-> (create-state "#############"
                                  "#DcBa.#.GhKl#"
                                  "#.###@#@#I###"
                                  "#e#d#####j#k#"
                                  "###C#@#@###J#"
                                  "#fEbA.#.FgHi#"
                                  "#############")
                    (create-graph))
                {0  {:a {:distance 2 :passing-keys #{} :required-keys #{}}
                     :c {:distance 4 :passing-keys #{:a} :required-keys #{:b}}
                     :e {:distance 7 :passing-keys #{:c :a} :required-keys #{:b :d}}}
                 :a {:c {:distance 2 :passing-keys #{} :required-keys #{:b}}
                     :e {:distance 5 :passing-keys #{:c} :required-keys #{:b :d}}}
                 :c {:a {:distance 2 :passing-keys #{} :required-keys #{:b}}
                     :e {:distance 3 :passing-keys #{} :required-keys #{:d}}}
                 :e {:c {:distance 3 :passing-keys #{} :required-keys #{:d}}
                     :a {:distance 5 :passing-keys #{:c} :required-keys #{:b :d}}}

                 1  {:h {:distance 3 :passing-keys #{} :required-keys #{:g}}
                     :l {:distance 5 :passing-keys #{:h} :required-keys #{:k :g}}
                     :j {:distance 5 :passing-keys #{:h} :required-keys #{:g :i}}}
                 :j {:h {:distance 2 :passing-keys #{} :required-keys #{:i}}
                     :l {:distance 4 :passing-keys #{:h} :required-keys #{:k :i}}}
                 :l {:h {:distance 2 :passing-keys #{} :required-keys #{:k}}
                     :j {:distance 4 :passing-keys #{:h} :required-keys #{:k :i}}}
                 :h {:l {:distance 2 :passing-keys #{} :required-keys #{:k}}
                     :j {:distance 2 :passing-keys #{} :required-keys #{:i}}}

                 2  {:b {:distance 3 :passing-keys #{} :required-keys #{:a}}
                     :f {:distance 5 :passing-keys #{:b} :required-keys #{:e :a}}
                     :d {:distance 5 :passing-keys #{:b} :required-keys #{:c :a}}}
                 :b {:f {:distance 2 :passing-keys #{} :required-keys #{:e}}
                     :d {:distance 2 :passing-keys #{} :required-keys #{:c}}}
                 :d {:b {:distance 2 :passing-keys #{} :required-keys #{:c}}
                     :f {:distance 4 :passing-keys #{:b} :required-keys #{:e :c}}}
                 :f {:b {:distance 2 :passing-keys #{} :required-keys #{:e}}
                     :d {:distance 4 :passing-keys #{:b} :required-keys #{:e :c}}}

                 3  {:g {:distance 3 :passing-keys #{} :required-keys #{:f}}
                     :i {:distance 5 :passing-keys #{:g} :required-keys #{:h :f}}
                     :k {:distance 7 :passing-keys #{:g :i} :required-keys #{:j :h :f}}}
                 :g {:i {:distance 2 :passing-keys #{} :required-keys #{:h}}
                     :k {:distance 4 :passing-keys #{:i} :required-keys #{:j :h}}}
                 :i {:g {:distance 2 :passing-keys #{} :required-keys #{:h}}
                     :k {:distance 2 :passing-keys #{} :required-keys #{:j}}}
                 :k {:i {:distance 2 :passing-keys #{} :required-keys #{:j}}
                     :g {:distance 4 :passing-keys #{:i} :required-keys #{:j :h}}}
                 }))}
  [state]
  (let [->position (reduce-kv assoc
                              (clojure.set/map-invert (:keys state))
                              (:start state))]
    (reduce (fn [a v]
              (assoc a v (get-keys-from-position state (->position v))))
            {}
            (concat (vals (:keys state)) (->> (seq (:start state))
                                              (map first))))))

(defn been-there-with-better-stats?
  {:test (fn []
           (is (been-there-with-better-stats? {[:a :b :c :d] {#{:b :c} [1 1 1 1]}}
                                              {:positions      [:a :b :c :d]
                                               :collected-keys #{:b :c}
                                               :movements      [1 3 4 5]}))
           (is-not (been-there-with-better-stats? {[:a :b :c :d] {#{:b :c} [100 1 1 1]}}
                                                  {:positions      [:a :b :c :d]
                                                   :collected-keys #{:b :c}
                                                   :movements      [1 3 4 5]}))
           (is-not (been-there-with-better-stats? {[:a :b :c :d] {#{:b} [1 1 1 1]
                                                                  #{:a} [1 1 1 1]}}
                                                  {:positions      [:a :b :c :d]
                                                   :collected-keys #{:b :c}
                                                   :movements      [1 3 4 5]}))
           (is (been-there-with-better-stats? {[:a :b :c :d] {#{:b :c :d} [1 1 1 1]}}
                                              {:positions      [:a :b :c :d]
                                               :collected-keys #{:b :c}
                                               :movements      [1 3 4 5]})))}
  [visits {positions      :positions
           collected-keys :collected-keys
           movements      :movements}]
  (->> (get visits positions)
       (seq)
       (filter (fn [[k m]]
                 (and (subset? collected-keys k)
                      (<= (apply + m) (apply + movements)))))
       (first)))

(defn collect-all-keys-iteration-step
  {:test (fn []
           (is= (let [graph (-> (create-state "#############"
                                              "#DcBa.#.GhKl#"
                                              "#.###@#@#I###"
                                              "#e#d#####j#k#"
                                              "###C#@#@###J#"
                                              "#fEbA.#.FgHi#"
                                              "#############")
                                (create-graph))]
                  (->> {:queued [{:positions      [0 1 2 3]
                                  :movements      [0 0 0 0]
                                  :collected-keys #{}}]}
                       (collect-all-keys-iteration-step graph)))
                {:queued   [{:positions      [:a 1 2 3]
                             :movements      [2 0 0 0]
                             :collected-keys #{:a}}]
                 [0 1 2 3] {#{} [0 0 0 0]}})
           (is= (let [graph (-> (create-state "#############"
                                              "#DcBa.#.GhKl#"
                                              "#.###@#@#I###"
                                              "#e#d#####j#k#"
                                              "###C#@#@###J#"
                                              "#fEbA.#.FgHi#"
                                              "#############")
                                (create-graph))]
                  (->> {:queued [{:positions      [0 1 2 3]
                                  :movements      [0 0 0 0]
                                  :collected-keys #{}}]}
                       (collect-all-keys-iteration-step graph)
                       (collect-all-keys-iteration-step graph)))
                {:queued    [{:positions      [:a 1 :b 3]
                              :movements      [2 0 3 0]
                              :collected-keys #{:b :a}}]
                 [0 1 2 3]  {#{} [0 0 0 0]}
                 [:a 1 2 3] {#{:a} [2 0 0 0]}}))}
  [graph visits]
  (loop [queued (:queued visits)
         visits (assoc visits :queued (list))]
    (if (empty? queued)
      visits
      (let [current (first queued)
            {positions :positions movements :movements collected-keys :collected-keys} current]
        (if (been-there-with-better-stats? visits current)
          (recur (rest queued) visits)
          (recur (rest queued)
                 (reduce (fn [visits robot-index]
                           (let [position (get positions robot-index)
                                 new-things-to-queue (->> (get graph position)
                                                          (reduce-kv (fn [queued goto-key {distance      :distance
                                                                                           passing-keys  :passing-keys
                                                                                           required-keys :required-keys}]
                                                                       (if (and (subset? required-keys collected-keys)
                                                                                (not (contains? collected-keys goto-key)))
                                                                         (conj queued
                                                                               {:positions      (assoc positions robot-index goto-key)
                                                                                :collected-keys (reduce (fn [collected-keys passing-key]
                                                                                                          (conj collected-keys passing-key))
                                                                                                        (conj collected-keys goto-key)
                                                                                                        passing-keys)
                                                                                :movements      (update movements
                                                                                                        robot-index
                                                                                                        (fn [x]
                                                                                                          (+ x distance)))})
                                                                         queued))
                                                                     []))]
                             (-> (assoc-in visits [positions collected-keys] movements)
                                 (update :queued (fn [queued] (reduce conj queued new-things-to-queue))))))
                         visits
                         (range 4))))))))


(defn find-min-path-to-all-keys
  {:test (fn []
           (is= (-> (create-state "#############"
                                  "#a...@#@..be#"
                                  "#############"
                                  "#c.A.@#@.Ad.#"
                                  "#############")
                    (create-graph)
                    (find-min-path-to-all-keys {:queued    []
                                                [0 1 2 3]  {#{} [0 0 0 0]}
                                                [:a 1 2 3] {#{:a :b :c :d :e} [10 100 1 1]}
                                                [:b 1 2 3] {#{:a :b :c :d :e} [2 4 5 10]}
                                                [:c 1 2 3] {#{:a :b :c :d :e} [100 1 1 1]}}))
                21))}
  [graph visits]
  (let [all-keys (->> (dissoc graph 0 1 2 3)
                      (keys)
                      (set))]
    (reduce (fn [result part-results]
              (let [movements (get part-results all-keys)]
                (if-not movements
                  result
                  (let [movement (apply + movements)]
                    (cond (not result) movement
                          (< movement result) movement
                          :else result)))))
            nil
            (vals visits))))


(defn collect-all-keys
  {:test (fn []
           (is= (let [graph (-> (create-state "#######"
                                              "#a.#Cd#"
                                              "##@#@##"
                                              "#######"
                                              "##@#@##"
                                              "#cB#.b#"
                                              "#######")
                                (create-graph))]
                  (->> graph
                       (collect-all-keys)))
                {:queued       []
                 [:a :d :c :b] {#{:c :b :d :a} [2 2 2 2]}
                 [0 :d :c :b]  {#{:c :b :d} [0 2 2 2]}
                 [:a 1 2 :b]   {#{:b :a} [2 0 0 2]}
                 [:a 1 :c :b]  {#{:c :b :a} [2 0 2 2]}
                 [0 1 :c :b]   {#{:c :b} [0 0 2 2]}
                 [0 1 2 :b]    {#{:b} [0 0 0 2]}
                 [:a 1 2 3]    {#{:a} [2 0 0 0]}
                 [0 1 2 3]     {#{} [0 0 0 0]}}))}
  [graph]
  (loop [result {:queued [{:positions      [0 1 2 3]
                           :collected-keys #{}
                           :movements      [0 0 0 0]}]}]
    (let [new-result (collect-all-keys-iteration-step graph result)]
      (if (= new-result result)
        result
        (recur new-result)))))

(defn solve-puzzle [& strings]
  (time (let [graph (-> (apply create-state strings)
                        (create-graph))]
          (->> (collect-all-keys graph)
               (find-min-path-to-all-keys graph)
               ))))


(comment

  (solve-puzzle "###############"
                "#d.ABC.#.....a#"
                "######@#@######"
                "###############"
                "######@#@######"
                "#b.....#.....c#"
                "###############")
  ; "Elapsed time: 1.819915 msecs"
  ; => 24


  (solve-puzzle "#############"
                "#DcBa.#.GhKl#"
                "#.###@#@#I###"
                "#e#d#####j#k#"
                "###C#@#@###J#"
                "#fEbA.#.FgHi#"
                "#############")
  ; "Elapsed time: 3.252424 msecs"
  ; => 32

  (solve-puzzle "#############"
                "#g#f.D#..h#l#"
                "#F###e#E###.#"
                "#dCba@#@BcIJ#"
                "#############"
                "#nK.L@#@G...#"
                "#M###N#H###.#"
                "#o#m..#i#jk.#"
                "#############")
  ; "Elapsed time: 12.206333 msecs"
  ; => 72

  (->> (slurp "src/advent_of_code/dec_2019/day_18b.txt")
       (clojure.string/split-lines)
       (apply solve-puzzle))
  ; "Elapsed time: 3365.278604 msecs"
  ; => 1732

  )

