(ns day-1-task-2.core
    (:require [clojure.string :as str])
    (:import (java.lang Character)))

(defmulti string->digit #(= 1 (count %)))

(defmethod string->digit true
    [s]
    (Integer/parseInt s))

(defmethod string->digit false
    [s]
    (case s
        "one" 1
        "two" 2
        "three" 3
        "four" 4
        "five" 5
        "six" 6
        "seven" 7
        "eight" 8
        "nine" 9))

(defn re-seq-pos [pattern string]
    (let [m (re-matcher pattern string)]
        ((fn step []
             (when (. m find)
                 (cons {:start (. m start) :end (. m end) :group (. m group)}
                       (lazy-seq (step))))))))

(defn match-digits [line]
    (concat
        (re-seq-pos #"one" line)
        (re-seq-pos #"two" line)
        (re-seq-pos #"three" line)
        (re-seq-pos #"four" line)
        (re-seq-pos #"five" line)
        (re-seq-pos #"six" line)
        (re-seq-pos #"seven" line)
        (re-seq-pos #"eight" line)
        (re-seq-pos #"nine" line)
        (re-seq-pos #"\d" line)))

(comment
    (match-digits "aslkfqjoneoaf1nqonea;lksfdokaf1")
    (sort-by :start (match-digits "eightwothree")))

(defn line->2digit [line]
    (->> line
         match-digits
         (sort-by :start)
         (map :group)
         (map string->digit)
         ((juxt first last))
         str/join
         Integer/parseInt))

(comment
    (line->2digit "1asdsdone")
    ((juxt first last) (filter #(Character/isDigit ^char %) (vec "as1da3d")))
    ((juxt first last) [1 2 3 4 5]))

(defn solve [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (reduce + (map line->2digit (line-seq rdr)))))

(comment
    (solve "resources/test1")
    (solve "resources/test2"))
