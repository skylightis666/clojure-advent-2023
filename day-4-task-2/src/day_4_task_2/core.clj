(ns day-4-task-2.core
    (:require [clojure.string :as str]))

(defn parse-inp-string [line]
    (mapv #(into [] (str/split % #"\s+"))
          (-> line
              (str/split #":\s+|\s+\|\s+")
              rest)))
(defn get-input [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (mapv parse-inp-string (into [] (line-seq rdr)))))

(defn winning->set [coll]
    (mapv #(update % 0 set) coll))

(defn count-ans-in-card [[win my] i]
    (->> (map win my)
         (filter identity)
         count
         (+ i)
         inc
         (range (inc i))))

(defn rec [coll]
    (loop [i 0
           acc (zipmap (range (count coll)) (repeat 1))]
        (if (= i (count coll))
            acc
            (recur (inc i) (reduce
                               #(update %1 %2 + (get acc i))
                               acc
                               (count-ans-in-card (get coll i) i))))))

(defn solve [file-name]
    (->> file-name
         get-input
         winning->set
         rec
         vals
         (reduce +)))
(comment
    (count-ans-in-card [#{"61" "20" "13" "32" "16"} ["61" "30" "68" "82" "17" "32" "24" "19"]] 1)
    (count-ans-in-card (get (winning->set (get-input "resources/test1")) 3) 3)
    (solve "resources/test1")
    (solve "resources/test2")
    (get-input "resources/test1")
    (parse-inp-string "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
    (->> "resources/test1"
         get-input
         winning->set
         rec
         vals
         (reduce +))
    (zipmap (range 6) (repeat 1))
    (update-in (zipmap (range 6) (repeat 1))
               (count-ans-in-card (get (winning->set (get-input "resources/test1")) 0) 0)
               + (get (zipmap (range 6) (repeat 1)) 0))
    (update-in {0 1, 1 1, 2 1, 3 1, 4 1, 5 1} [1 2] + 1)
    (zipmap [1 2 3 4] (repeat 0)))

(comment
    (solve "resources/test1")
    (solve "resources/test2")
    )