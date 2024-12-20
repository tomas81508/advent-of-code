(ns advent-of-code.dec-2016.day-17
  (:require [advent-of-code.test :refer [is= is is-not]]
            [digest :refer [md5]]))

(def input "hhhxzeay")

(defn open? [c] (contains? #{\b \c \d \e \f} c))

(defn create-state
  [passcode]
  {:paths            [{:position [0 0] :passcode passcode}]
   :initial-passcode passcode})

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step {:position [0 0] :passcode "hijkl"})
                #{{:position [0 1] :passcode "hijklD"}})
           (is= (walk-a-step {:position [0 1] :passcode "hijklD"})
                #{{:position [0 0] :passcode "hijklDU"}
                  {:position [1 1] :passcode "hijklDR"}}))}
  [{[x y :as p] :position passcode :passcode}]
  (let [[u d l r] (->> (md5 passcode)
                       (take 4))]
    (->> (cond-> #{}
                 (and (open? u) (pos? y))
                 (conj {:position (mapv + p [0 -1]) :passcode (str passcode "U")})

                 (and (open? d) (< y 3))
                 (conj {:position (mapv + p [0 1]) :passcode (str passcode "D")})

                 (and (open? l) (pos? x))
                 (conj {:position (mapv + p [-1 0]) :passcode (str passcode "L")})

                 (and (open? r) (< x 3))
                 (conj {:position (mapv + p [1 0]) :passcode (str passcode "R")})))))

(defn walk
  {:test (fn []
           (is= (walk (create-state "ihgpwlah")) "DDRRRD")
           (is= (walk (create-state "kglvqrro")) "DDUDRLRRUDRD"))}
  [state]
  (loop [paths (:paths state)]
    (let [vault (->> paths
                     (filter (fn [{p :position}] (= p [3 3]))))]
      (if-not (empty? vault)
        (->> (:passcode (first vault))
             (drop (count (:initial-passcode state)))
             (apply str))
        (let [new-paths (->> paths
                             (map walk-a-step)
                             (apply clojure.set/union))]
          (recur new-paths))))))

(comment
  (time (walk (create-state "hhhxzeay")))
  )

(defn walk-2
  {:test (fn []
           (is= (walk-2 (create-state "ihgpwlah")) 370)
           (is= (walk-2 (create-state "kglvqrro")) 492)
           (is= (walk-2 (create-state "ulqzkmiv")) 830))}
  [state]
  (loop [paths (:paths state)
         steps 0
         longest 0]
    (if (empty? paths)
      longest
      (let [new-paths (->> paths
                           (map walk-a-step)
                           (apply clojure.set/union))
            vault (->> new-paths
                       (filter (fn [{p :position}] (= p [3 3]))))]
        (recur (->> new-paths
                    (remove (fn [{p :position}] (= p [3 3]))))
               (inc steps)
               (if (empty? vault) longest (inc steps)))))))

(comment
  (time (walk-2 (create-state "hhhxzeay")))
  )