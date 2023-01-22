(ns advent-of-code.dec-2022.day-07
  (:require [ysera.test :refer [is is-not is=]]
            [clojure.test :refer [deftest]]
            [clojure.string :refer [split-lines
                                    starts-with?]]))

(def input (->> (slurp "src/advent_of_code/dec_2022/day_07_input.txt")
                (split-lines)))

(def test-input (->> "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
                     (split-lines)))

(defn create-input-data
  {:test (fn []
           (is= (create-input-data test-input)
                [{:command "cd /"}
                 {:command "ls", :result ["dir a" "14848514 b.txt" "8504156 c.dat" "dir d"]}
                 {:command "cd a"}
                 {:command "ls", :result ["dir e" "29116 f" "2557 g" "62596 h.lst"]}
                 {:command "cd e"}
                 {:command "ls", :result ["584 i"]}
                 {:command "cd .."}
                 {:command "cd .."}
                 {:command "cd d"}
                 {:command "ls" :result ["4060174 j" "8033020 d.log" "5626152 d.ext" "7214296 k"]}]))}
  [input]
  (loop [[i & ir] input
         current-command nil
         current-result []
         result []]
    (cond (not i)
          (conj result (merge {:command current-command}
                              (when-not (empty? current-result)
                                {:result current-result})))

          (and (starts-with? i "$") (not current-command))
          (recur ir (subs i 2) current-result result)

          (starts-with? i "$")
          (recur ir (subs i 2) [] (conj result (merge {:command current-command}
                                                      (when-not (empty? current-result)
                                                        {:result current-result}))))

          :else
          (recur ir current-command (conj current-result i) result))))

(def test-input-data (create-input-data test-input))

(def input-data (create-input-data input))

(def empty-state {:directories       {[:/] {}}
                  :current-directory [:/]})

(defn handle-commands
  {:test (fn []
           (is= (handle-commands {:directories       {[:/] {}}
                                  :current-directory [:/]}
                                 [{:command "cd /"}
                                  {:command "ls", :result ["14848514 b.txt"]}])
                {:directories       {[:/] {:files #{{:name "b.txt" :size 14848514}}}}
                 :current-directory [:/]})
           (is= (handle-commands {:directories       {[:/] {}}
                                  :current-directory [:/]}
                                 [{:command "cd /"}
                                  {:command "ls", :result ["dir a" "14848514 b.txt" "8504156 c.dat" "dir d"]}
                                  {:command "cd a"}
                                  {:command "ls", :result ["dir e" "29116 f" "2557 g" "62596 h.lst"]}
                                  {:command "cd e"}
                                  {:command "ls", :result ["584 i"]}
                                  {:command "cd .."}
                                  {:command "cd .."}
                                  {:command "cd d"}
                                  {:command "ls" :result ["4060174 j" "8033020 d.log" "5626152 d.ext" "7214296 k"]}])
                {:directories       {[:/]       {:files #{{:name "c.dat" :size 8504156}
                                                          {:name "b.txt" :size 14848514}}}
                                     [:/ :a]    {:files #{{:name "h.lst" :size 62596}
                                                          {:name "g" :size 2557}
                                                          {:name "f" :size 29116}}}
                                     [:/ :a :e] {:files #{{:name "i" :size 584}}}
                                     [:/ :d]    {:files #{{:name "k", :size 7214296}
                                                          {:name "d.ext", :size 5626152}
                                                          {:name "d.log", :size 8033020}
                                                          {:name "j", :size 4060174}}}}
                 :current-directory [:/ :d]}))}
  [state commands]
  (loop [[c & cs] commands
         state state]
    (if-not c
      state
      (recur cs
             (cond (= (:command c) "cd /")
                   (assoc state :current-directory [:/])

                   (= (:command c) "cd ..")
                   (update state :current-directory (fn [cds] (vec (drop-last 1 cds))))

                   (starts-with? (:command c) "cd ")
                   (let [directory (subs (:command c) 3)]
                     (update state :current-directory conj (keyword directory)))

                   (= (:command c) "ls")
                   (reduce (fn [state result-part]
                             (if (starts-with? result-part "dir")
                               (update-in state [:directories (conj (:current-directory state) (keyword (subs result-part 4)))]
                                          (fn [d] (or d {})))
                               (let [[_ size n] (re-find #"(\d+) (.*)" result-part)]
                                 (update-in state [:directories (:current-directory state) :files]
                                            (fn [files] (set (conj files {:name n :size (read-string size)})))))))
                           state
                           (:result c)))))))

(def state (handle-commands empty-state input-data))

(defn get-size-of-directory
  {:test (fn []
           (is= (get-size-of-directory {[:/]       {:files #{{:name "c.dat" :size 8504156}
                                                             {:name "b.txt" :size 14848514}}}
                                        [:/ :a]    {:files #{{:name "h.lst" :size 62596}
                                                             {:name "g" :size 2557}
                                                             {:name "f" :size 29116}}}
                                        [:/ :a :e] {:files #{{:name "i" :size 584}}}
                                        [:/ :d]    {:files #{{:name "k", :size 7214296}
                                                             {:name "d.ext", :size 5626152}
                                                             {:name "d.log", :size 8033020}
                                                             {:name "j", :size 4060174}}}}
                                       [:/ :a])
                94853))}
  [directories path]
  (->> (seq directories)
       (filter (fn [[k _]]
                 (loop [[fp & rp] path
                        [fk & rk] k]
                   (cond (not fp) true
                         (= fp fk) (recur rp rk)
                         :else false))))
       (map (fn [[_ {files :files}]]
              (->> files
                   (map :size)
                   (apply +))))
       (apply +)))


(defn get-size-of-directories
  {:test (fn []
           (is= (get-size-of-directories {[:/]       {:files #{{:name "c.dat" :size 8504156}
                                                               {:name "b.txt" :size 14848514}}}
                                          [:/ :a]    {:files #{{:name "h.lst" :size 62596}
                                                               {:name "g" :size 2557}
                                                               {:name "f" :size 29116}}}
                                          [:/ :a :e] {:files #{{:name "i" :size 584}}}
                                          [:/ :d]    {:files #{{:name "k", :size 7214296}
                                                               {:name "d.ext", :size 5626152}
                                                               {:name "d.log", :size 8033020}
                                                               {:name "j", :size 4060174}}}})
                {[:/]       48381165
                 [:/ :a]    94853
                 [:/ :a :e] 584
                 [:/ :d]    24933642})
           )}
  [directories]
  (reduce-kv (fn [a k _]
               (assoc a k (get-size-of-directory directories k)))
             {}
             directories))

(def directories-size (get-size-of-directories (:directories state)))

(comment

  (->> (seq directories-size)
       (keep (fn [[_ size]] (when (<= size 100000) size)))
       (apply +))
  ; 249695773 too high

  )

(def total-disc-space 70000000)

(def needed-space 30000000)

(def used-disc-space (get directories-size [:/]))

(def available-disc-space (- total-disc-space used-disc-space))

(def missing-space (- needed-space available-disc-space))

(comment
  (->> (vals directories-size)
       (reduce (fn [a size]
                 (if (and (>= size missing-space)
                          (< size a))
                   size
                   a))
               total-disc-space))
  )


