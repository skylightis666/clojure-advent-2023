(ns day-5-task-1.core
    (:require [clojure.string :as str]))

(defn parse-int [val]
    (Long/parseLong val))

(defn parse-int-vec [s]
    (mapv parse-int (str/split s #"\s+")))

(defn rest->int-vecs [coll]
    (map parse-int-vec (rest coll)))
(defn get-input [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (let [fl (into [] (line-seq rdr))
              seeds (as-> fl a
                          (first a)
                          (str/split a #":\s+|\s+")
                          (rest a)
                          (map parse-int a)
                          )
              mappings (->> fl
                            (drop 2)
                            (partition-by #(= 0 (count %)))
                            (filter #(not= '("") %))
                            (map rest->int-vecs))]
            {:seeds seeds :mappings mappings})))

(defn one-src->dest [src mapping]
    (let [mapping-shift (for [[dest-start src-start range-len] mapping
                              :when (and (< src (+ src-start range-len)) (>= src src-start))] ;while?
                            (- dest-start src-start))]
        (if (empty? mapping-shift)
            src
            (+ src (first mapping-shift)))))

(defn vec-src->dest [vec mapping]
    (set (map #(one-src->dest % mapping) vec)))

(defn multiply-seeds [seeds]
    (->> seeds
         (partition 2 2)
         (map #(range (first %) (+ (first %) (second %))))
         (flatten)
         (set)
         ))

(defn rofl-seeds [seeds]
    (->> seeds
         (partition 2 2)
         (map #(conj [(first %)] (+ (first %) (second %))))
         (flatten)
         ))

(defn solve [file-name]
    (let [{seeds :seeds
           mappings :mappings} (get-input file-name)]
        (reduce min (reduce vec-src->dest (rofl-seeds seeds) mappings))))

(comment
    (get-input "resources/test1")
    (rest->int-vecs '("seed-to-soil map:" "50 98 2" "52 50 48"))
    (one-src->dest 79 '([50 98 2] [52 50 48]))
    (vec-src->dest [79 14 55 13] '([50 98 2] [52 50 48]))
    (solve "resources/test1")
    (solve "resources/test2")
    (multiply-seeds [79 14 55 13])
    (rofl-seeds [79 14 55 13]))