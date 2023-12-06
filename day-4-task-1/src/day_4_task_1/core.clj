(ns day-4-task-1.core
    (:require [clojure.string :as str]))

(defn parse-inp-string [line]
    (mapv #(into [] (str/split % #"\s+"))
         (-> line
             (str/split #":\s+|\s+\|\s+")
             rest)))
(defn get-input [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (map parse-inp-string (into [] (line-seq rdr)))))

(defn winning->set [coll]
    (map #(update % 0 set) coll))

(defn count-ans-in-card [[win my]]
    (->> (map win my)
         (filter identity)
         count
         dec
         (Math/pow 2)
         Math/floor
         int))

(defn solve [file-name]
    (->> file-name
         get-input
         winning->set
         (map count-ans-in-card)
         (reduce +)))
(comment
    (solve "resources/test1")
    (solve "resources/test2")
    (get-input "resources/test1")
    (parse-inp-string "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
    (into #{} [1 2 3])
    (#(update % 0 set) ["83" "86" "6" "31" "17" "9" "48" "53"]))
