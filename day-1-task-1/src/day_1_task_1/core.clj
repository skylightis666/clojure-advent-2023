(ns day-1-task-1.core
    (:import [java.lang Character])
    (:require [clojure.string :as str]))


(defn line->2digit [line]
    (->> line
         vec
         (filter #(Character/isDigit ^char %))
         ((juxt first last))
         str/join
         Integer/parseInt))

(comment
    (line->2digit "1asdsd3")
    ((juxt first last) (filter #(Character/isDigit ^char %) (vec "as1da3d")))
    ((juxt first last) [1 2 3 4 5]))

(defn solve [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (reduce + (map line->2digit (line-seq rdr)))))

(comment
    (solve "resources/test1")
    (solve "resources/test2"))
