(ns day-2-task-1.core
    (:require [clojure.string :as str]))

(defn input-line->map [input]
    (let [[game-str sets-str] (str/split input #": ")
          game-map {:game (Integer/parseInt (last (str/split game-str #" ")))}
          sets (map
                       (comp (partial map (fn [[frst scnd]] (list scnd (Integer/parseInt frst))))
                             (partial partition 2 2))
                       (map
                           #(str/split % #", | ")
                           (str/split sets-str #"; ")))
          parsed-line (assoc game-map :sets sets)]
        parsed-line))

(defn set-possible? [bag set]
    (every? true? (map #(<= (second %) (get bag (first %))) set)))

(defn sets-possible? [bag line]
    (let [ans (every? true? (map (partial set-possible? bag) (:sets line)))]
        (if ans
            (:game line)
            0)))

(def bag {"red" 12
          "green" 13
          "blue" 14})

(comment
    (set-possible? bag '(("blue" 3) ("red" 4)))
    (sets-possible? bag (input-line->map "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
    (sets-possible? bag (input-line->map "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"))
    ((comp input-line->map) "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
    (sets-possible? bag {:game 3, :sets ((("green" 8) ("blue" 6) ("red" 20)) (("blue" 5) ("red" 4) ("green" 13)) (("green" 5) ("red" 1)))})
    (input-line->map "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    ((fn [[frst scnd]] {scnd frst}) '("asd" "d"))
    (map (comp (partial map (fn [[frst scnd]] {scnd frst})) (partial partition 2 2)) (map #(str/split % #", | ") (str/split "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" #"; "))))

(defn solve [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (reduce + (map (comp (partial sets-possible? bag) input-line->map) (line-seq rdr)))))

(comment
    (solve "resources/test1")
    (solve "resources/test2"))