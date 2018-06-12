(ns advent-of-code.dec-2017.day-07
  (:require [ysera.test :refer [deftest is=]]
            [ysera.collections :refer [seq-contains?]]
            [clojure.string :as string]))

(def test-data (->> "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"
                    (string/split-lines)))

(defn get-targets
  {:test (fn []
           (is= (get-targets ["ktlj (57)"
                              "fwft (72) -> ktlj, cntj, xhth"])
                ["ktlj" "cntj" "xhth"]))}
  [data]
  )


