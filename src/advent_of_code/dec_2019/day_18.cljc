(ns advent-of-code.dec-2019.day-18
  (:require [ysera.test :refer [is= is is-not]]))

(defn create-state
  {:test (fn []
           (is= (create-state "#########"
                              "#b.A.@.a#"
                              "#########")
                {:movement     0
                 :visits       {[5 1] [{:collected-keys #{}
                                        :movement       0}]}
                 :walls        #{[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] [8 0]
                                 [0 1] [8 1]
                                 [0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2] [7 2] [8 2]}
                 :open-passage #{[2 1] [4 1] [6 1] [5 1]}
                 :keys         {[1 1] "b"
                                [7 1] "a"}
                 :doors        {[3 1] "A"}}))}
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
                       (let [stats {:collected-keys #{}
                                    :movement       0}]
                         (-> a
                             (update :open-passage conj position)
                             (assoc :visits {position [stats]})))

                       (= l \#)
                       (update a :walls conj position)

                       (= l \.)
                       (update a :open-passage conj position)

                       (re-matches #"[a-z]" (str l))
                       (update a :keys assoc position (str l))

                       (re-matches #"[A-Z]" (str l))
                       (update a :doors assoc position (str l))))
               {:movement     0
                :walls        #{}
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

(defn get-positions-with-highest-movement
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (get-positions-with-highest-movement))
                [[5 1]]))}
  [state]
  (let [movement (:movement state)]
    (reduce-kv (fn [a k vs]
                 (if (first (filter (fn [v] (= (:movement v) movement)) vs))
                   (conj a k)
                   a))
               []
               (:visits state))))

(defn move-one-step-in-all-directions-from-position
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (move-one-step-in-all-directions-from-position [5 1])
                    (:visits))
                {[5 1] [{:collected-keys #{}
                         :movement       0}]
                 [6 1] [{:collected-keys #{}
                         :movement       1}]
                 [4 1] [{:collected-keys #{}
                         :movement       1}]})
           (is= (-> (create-state "#########"
                                  "#b.A..@a#"
                                  "#########")
                    (move-one-step-in-all-directions-from-position [6 1])
                    (:visits))
                {[7 1] [{:collected-keys #{"a"}
                         :movement       1}]
                 [6 1] [{:collected-keys #{}
                         :movement       0}]
                 [5 1] [{:collected-keys #{}
                         :movement       1}]})
           (is= (-> (create-state "#########"
                                  "#b.A@..a#"
                                  "#########")
                    (update-in [:visits [4 1] 0 :collected-keys] conj "a")
                    (move-one-step-in-all-directions-from-position [4 1])
                    (:visits))
                {[3 1] [{:collected-keys #{"a"}
                         :movement       1}]
                 [4 1] [{:collected-keys #{"a"}
                         :movement       0}]
                 [5 1] [{:collected-keys #{"a"}
                         :movement       1}]})
           (is= (-> (create-state "#########"
                                  "#b.A@..a#"
                                  "#########")
                    (move-one-step-in-all-directions-from-position [4 1])
                    (:visits))
                {[4 1] [{:collected-keys #{}
                         :movement       0}]
                 [5 1] [{:collected-keys #{}
                         :movement       1}]})
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (move-one-step-in-all-directions-from-position [5 1])
                    (update :movement inc)
                    (move-one-step-in-all-directions-from-position [6 1])
                    (:visits))
                {[6 1] [{:collected-keys #{}
                         :movement       1}]
                 [5 1] [{:collected-keys #{}
                         :movement       0}]
                 [4 1] [{:collected-keys #{}
                         :movement       1}]
                 [7 1] [{:collected-keys #{"a"}
                         :movement       2}]})
           (is= (-> (create-state "#########"
                                  "#b.A..@a#"
                                  "#########")
                    (move-one-step-in-all-directions-from-position [6 1])
                    (update :movement inc)
                    (move-one-step-in-all-directions-from-position [7 1])
                    (:visits))
                {[6 1] [{:collected-keys #{"a"}
                         :movement       2}]
                 [5 1] [{:collected-keys #{}
                         :movement       1}]
                 [7 1] [{:collected-keys #{"a"}
                         :movement       1}]})
           (is= (-> (create-state "######"
                                  "#@.a#"
                                  "######")
                    (assoc-in [:visits [2 1]] [{:collected-keys #{"a"} :movement 1}])
                    (update-in [:visits [1 1] 0 :collected-keys] conj "b")
                    (move-one-step-in-all-directions-from-position [1 1])
                    (:visits)
                    (get [2 1]))
                [{:collected-keys #{"a"}
                  :movement       1}
                 {:collected-keys #{"b"}
                  :movement       1}]))}
  [state position]
  (let [new-positions (->> [[-1 0] [1 0] [0 -1] [0 1]]
                           (map (fn [d] (mapv + position d)))
                           (remove (partial wall? state)))
        movement (:movement state)]
    (->> (get-in state [:visits position])
         (filter (fn [{m :movement}] (= movement m)))
         (reduce (fn [state visit-stat]
                   (reduce (fn [state new-position]
                             (let [stats-for-new-position (get-in state [:visits new-position])
                                   new-stat (-> (if (key? state new-position)
                                                  (update visit-stat :collected-keys conj (get-in state [:keys new-position]))
                                                  visit-stat)
                                                (update :movement inc))]

                               (cond (when-let [door (get-in state [:doors new-position])]
                                       (let [door-key (clojure.string/lower-case door)]
                                         (not (contains? (:collected-keys visit-stat) door-key))))
                                     state

                                     (->> stats-for-new-position
                                          (filter (fn [{collected-keys :collected-keys}]
                                                    (clojure.set/subset? (:collected-keys visit-stat)
                                                                         collected-keys)))
                                          (first))
                                     state

                                     (->> stats-for-new-position
                                          (filter (fn [{collected-keys :collected-keys}]
                                                    (clojure.set/subset? collected-keys
                                                                         (:collected-keys visit-stat))))
                                          (first))
                                     (let [history-stat (->> stats-for-new-position
                                                             (filter (fn [{collected-keys :collected-keys}]
                                                                       (clojure.set/subset? collected-keys
                                                                                            (:collected-keys visit-stat))))
                                                             (first))]
                                       (update-in state [:visits new-position]
                                                  (fn [history] (replace {history-stat new-stat} history))))


                                     :else
                                     (update-in state [:visits new-position] conj new-stat))))
                           state
                           new-positions))
                 state))))

(defn state->string
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (state->string))
                "#########\n#b.A.@.a#\n#########"))}
  [state]
  (let [max-wall-x (reduce max (map first (:walls state)))
        max-wall-y (reduce max (map second (:walls state)))]
    (->> (for [y (range (inc max-wall-y)) x (range (inc max-wall-x))] [x y])
         (partition (inc max-wall-x))
         (map (fn [row]
                (->> row
                     (map (fn [position]
                            (let [any-at-current (->> (get-in state [:visits position])
                                                      (filter (fn [{movement :movement}]
                                                                (= movement (:movement state))))
                                                      (first))]
                              (cond any-at-current "@"

                                    (wall? state position) "#"

                                    (open-passage? state position) "."

                                    (contains? (:keys state) position)
                                    (get-in state [:keys position])

                                    (contains? (:doors state) position)
                                    (get-in state [:doors position])))))
                     (clojure.string/join))))
         (clojure.string/join "\n"))))


(defn collect-all-keys
  {:test (fn []
           (is= (-> (create-state "#########"
                                  "#b.A.@.a#"
                                  "#########")
                    (collect-all-keys)
                    (:movement))
                8)
           (is= (-> (create-state "########################"
                                  "#f.D.E.e.C.b.A.@.a.B.c.#"
                                  "######################.#"
                                  "#d.....................#"
                                  "########################")
                    (collect-all-keys)
                    (:movement))
                86)
           (is= (-> (create-state "########################"
                                  "#...............b.C.D.f#"
                                  "#.######################"
                                  "#.....@.a.B.c.d.A.e.F.g#"
                                  "########################")
                    (collect-all-keys)
                    (:movement))
                132)
           (is= (-> (create-state "#################"
                                  "#i.G..c...e..H.p#"
                                  "########.########"
                                  "#j.A..b...f..D.o#"
                                  "########@########"
                                  "#k.E..a...g..B.n#"
                                  "########.########"
                                  "#l.F..d...h..C.m#"
                                  "#################")
                    (collect-all-keys)
                    (:movement))
                136)
           (is= (-> (create-state "########################"
                                  "#@..............ac.GI.b#"
                                  "###d#e#f################"
                                  "###A#B#C################"
                                  "###g#h#i################"
                                  "########################")
                    (collect-all-keys)
                    (:movement))
                81))}
  [state]
  (let [keys-to-collect (into #{} (vals (:keys state)))]
    (loop [state state
           index 0]
      (let [current-positions (get-positions-with-highest-movement state)]
        (if (or (empty? current-positions)
                (->> current-positions
                     (some (fn [cp]
                             (->> (get-in state [:visits cp])
                                  (filter (fn [{collected-keys :collected-keys}]
                                            (= keys-to-collect collected-keys)))
                                  (first))))))
          state
          (recur (-> (reduce move-one-step-in-all-directions-from-position
                             state
                             current-positions)
                     (update :movement inc))
                 (inc index)))))))

(comment
  (time (->> (slurp "src/advent_of_code/dec_2019/day_18.txt")
             (clojure.string/split-lines)
             (apply create-state)
             (collect-all-keys)
             (:movement)))
  ; 3048

  )
