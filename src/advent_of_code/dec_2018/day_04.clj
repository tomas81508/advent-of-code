(ns advent-of-code.dec-2018.day-04
  (:require [advent-of-code.test :refer [is= is is-not]]
            [clojure.test :refer [deftest]]))


(defn get-puzzle-input []
  (->> (slurp "src/advent_of_code/dec_2018/day_04.txt")
       (clojure.string/split-lines)))

(defn sort-logs
  {:test (fn []
           (is= (sort-logs ["[1518-07-01 00:47] wakes up"
                            "[1518-10-17 00:13] falls asleep"
                            "[1518-10-12 00:31] wakes up"
                            "[1518-09-24 00:03] Guard #2137 begins shift"
                            "[1518-10-11 00:35] wakes up"
                            "[1518-07-10 00:59] wakes up"])
                ["[1518-07-01 00:47] wakes up"
                 "[1518-07-10 00:59] wakes up"
                 "[1518-09-24 00:03] Guard #2137 begins shift"
                 "[1518-10-11 00:35] wakes up"
                 "[1518-10-12 00:31] wakes up"
                 "[1518-10-17 00:13] falls asleep"]))}
  [logs]
  (sort logs))

(comment (->> (get-puzzle-input)
              (sort-logs)))

(defn guard-begins-shift?
  {:test (fn []
           (is (guard-begins-shift? "[1518-11-01 00:00] Guard #10 begins shift"))
           (is-not (guard-begins-shift? "[1518-11-01 00:05] falls asleep"))
           (is-not (guard-begins-shift? "[1518-11-01 00:25] wakes up")))}
  [log]
  (clojure.string/index-of log "begins shift"))


(defn guard-falls-asleep?
  {:test (fn []
           (is-not (guard-falls-asleep? "[1518-11-01 00:00] Guard #10 begins shift"))
           (is (guard-falls-asleep? "[1518-11-01 00:05] falls asleep"))
           (is-not (guard-falls-asleep? "[1518-11-01 00:25] wakes up")))}
  [log]
  (clojure.string/index-of log "falls asleep"))


(defn get-guard
  {:test (fn []
           (is= (get-guard "[1518-11-01 00:00] Guard #10 begins shift")
                "#10"))}
  [log]
  (re-find (re-pattern "#[0-9]+") log))

(defn get-time-stamp
  {:test (fn []
           (is= (get-time-stamp "[1518-11-01 00:00] Guard #10 begins shift")
                "1518-11-01 00:00"))}
  [log]
  (re-find (re-pattern "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}") log))

(defn logs->data
  {:test (fn []
           (is= (logs->data ["[1518-11-01 00:00] Guard #10 begins shift"
                             "[1518-11-01 00:05] falls asleep"
                             "[1518-11-01 00:25] wakes up"
                             "[1518-11-01 00:30] falls asleep"
                             "[1518-11-01 00:55] wakes up"
                             "[1518-11-01 23:58] Guard #99 begins shift"
                             "[1518-11-02 00:40] falls asleep"
                             "[1518-11-02 00:50] wakes up"
                             "[1518-11-03 00:05] Guard #10 begins shift"
                             "[1518-11-03 00:24] falls asleep"
                             "[1518-11-03 00:29] wakes up"
                             "[1518-11-04 00:02] Guard #99 begins shift"
                             "[1518-11-04 00:36] falls asleep"
                             "[1518-11-04 00:46] wakes up"
                             "[1518-11-05 00:03] Guard #99 begins shift"
                             "[1518-11-05 00:45] falls asleep"
                             "[1518-11-05 00:55] wakes up"])
                {"#10" {"1518-11-01 00:00" [["1518-11-01 00:05" "1518-11-01 00:25"]
                                            ["1518-11-01 00:30" "1518-11-01 00:55"]]
                        "1518-11-03 00:05" [["1518-11-03 00:24" "1518-11-03 00:29"]]}
                 "#99" {"1518-11-01 23:58" [["1518-11-02 00:40" "1518-11-02 00:50"]]
                        "1518-11-04 00:02" [["1518-11-04 00:36" "1518-11-04 00:46"]]
                        "1518-11-05 00:03" [["1518-11-05 00:45" "1518-11-05 00:55"]]}}))}
  [logs]
  (-> (reduce (fn [{data :data guard :guard shift :shift asleep :asleep} log]
                (let [time-stamp (get-time-stamp log)]
                  (cond (guard-begins-shift? log)
                        (let [guard (get-guard log)]
                          {:data  (-> (if (contains? data guard)
                                        data
                                        (assoc data guard {}))
                                      (assoc-in [guard time-stamp] []))
                           :guard guard
                           :shift time-stamp})

                        (guard-falls-asleep? log)
                        {:data   data
                         :guard  guard
                         :shift  shift
                         :asleep time-stamp}

                        :else
                        {:data  (update-in data
                                           [guard shift]
                                           (fn [value]
                                             (conj value [asleep time-stamp])))
                         :guard guard
                         :shift shift})))
              {}
              (sort-logs logs))
      (:data)))

(defn get-interval-time
  {:test (fn []
           (is= (get-interval-time ["1518-11-01 00:05" "1518-11-01 00:25"])
                20))}
  [interval]
  (as-> interval $
        (map (fn [x] (.getTime (.parse (java.text.SimpleDateFormat. "yyyy-mm-dd hh:mm") x))) $)
        (apply - $)
        (/ $ (* 1000 60))
        (- $)))

(defn get-minutes-asleep
  {:test (fn []
           (is= (get-minutes-asleep {"1518-11-01 00:00" [["1518-11-01 00:05" "1518-11-01 00:25"]
                                                         ["1518-11-01 00:30" "1518-11-01 00:55"]]
                                     "1518-11-03 00:05" [["1518-11-03 00:24" "1518-11-03 00:29"]]})
                50))}
  [guard-data]
  (->> (vals guard-data)
       (flatten)
       (partition 2)
       (map get-interval-time)
       (apply +)))

(defn find-guard-with-most-minutes-asleep
  {:test (fn []
           (is= (find-guard-with-most-minutes-asleep {"#10" {"1518-11-01 00:00" [["1518-11-01 00:05" "1518-11-01 00:25"]
                                                                                 ["1518-11-01 00:30" "1518-11-01 00:55"]]
                                                             "1518-11-03 00:05" [["1518-11-03 00:24" "1518-11-03 00:29"]]}
                                                      "#99" {"1518-11-01 23:58" [["1518-11-02 00:40" "1518-11-02 00:50"]]
                                                             "1518-11-04 00:02" [["1518-11-04 00:36" "1518-11-04 00:46"]]
                                                             "1518-11-05 00:03" [["1518-11-05 00:45" "1518-11-05 00:55"]]}})
                "#10"))}
  [data]
  (-> (reduce-kv (fn [leader k v]
                   (let [minutes-asleep (get-minutes-asleep v)]
                     (if (> minutes-asleep (:minutes-asleep leader))
                       {:guard          k
                        :minutes-asleep minutes-asleep}
                       leader)))
                 {:guard          nil
                  :minutes-asleep 0}
                 data)
      (:guard)))

(defn get-minutes
  {:test (fn []
           (is= (get-minutes "1518-11-01 00:05") 5)
           (is= (get-minutes "1518-11-01 00:08") 8)
           (is= (get-minutes "1518-11-01 00:55") 55))}
  [time-log]
  (-> (re-pattern ":(0?)(\\d+)")
      (re-find time-log)
      (last)
      (read-string)))

(defn find-the-minute-when-the-guard-spend-asleep-the-most
  {:test (fn []
           (is= (find-the-minute-when-the-guard-spend-asleep-the-most
                  {"1518-11-01 00:00" [["1518-11-01 00:05" "1518-11-01 00:25"]
                                       ["1518-11-01 00:30" "1518-11-01 00:55"]]
                   "1518-11-03 00:05" [["1518-11-03 00:24" "1518-11-03 00:29"]]})
                {:minute 24 :number-of-times 2}))}
  [guard-data]
  (let [sleeping-intervals (->> (vals guard-data)
                                (flatten)
                                (partition 2)
                                (map (fn [interval] (map get-minutes interval))))
        [minute number-of-times] (->> (range 60)
                                      (map (fn [minute]
                                             (reduce (fn [number-of-times [falls-asleep wakes-up]]
                                                       (if (and (<= falls-asleep minute)
                                                                (< minute wakes-up))
                                                         (inc number-of-times)
                                                         number-of-times))
                                                     0
                                                     sleeping-intervals)))
                                      (map-indexed (fn [index number-of-times] [index number-of-times]))
                                      (sort-by second)
                                      (last))]
    {:minute minute :number-of-times number-of-times}))


(deftest puzzle-part-1
         (is= (let [data (->> (get-puzzle-input)
                              (logs->data))
                    guard (find-guard-with-most-minutes-asleep data)
                    minute (:minute (find-the-minute-when-the-guard-spend-asleep-the-most (get data guard)))]
                (* minute (read-string (subs guard 1))))
              85296))

(deftest puzzle-part-2
         (is= (let [data (->> (get-puzzle-input)
                              (logs->data))
                    {guard  :guard
                     minute :minute} (reduce-kv (fn [a guard guard-data]
                                                  (let [{minute          :minute
                                                         number-of-times :number-of-times}
                                                        (find-the-minute-when-the-guard-spend-asleep-the-most guard-data)]
                                                    (if (> number-of-times (:number-of-times a))
                                                      {:guard guard :number-of-times number-of-times :minute minute}
                                                      a)))
                                                {:guard nil :number-of-times 0 :minute nil}
                                                data)]
                (* minute (read-string (subs guard 1))))
              58559))
