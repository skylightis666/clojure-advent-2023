(ns day-2-task-2.core
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

(defn game-power [set]
    (loop [acc {:blue  0
                :red   0
                :green 0}
           set set]
        (if (empty? set)
            (reduce * (vals acc))
            (let [[color val] (first set)]
                (recur (update acc (keyword color) max val) (rest set))))))

(comment
    (keyword "blue")
    (partition 2 2 (flatten '((("blue" 3) ("red" 4)) (("red" 1) ("green" 2) ("blue" 6)) (("green" 2)))))
    (input-line->map "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
    (game-power '(("blue" 3) ("red" 4) ("red" 1) ("green" 2) ("blue" 6) ("green" 2)))
    (rest '(1))
    (rest '())
    (empty? '())
    (vals {:blue  0
           :red   0
           :green 0})
    (update {:blue  0
             :red   0
             :green 0}
            :green max 5))

(defn solve [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (->> rdr
             line-seq
             (map (comp game-power
                        (partial partition 2 2)
                        flatten
                        :sets
                        input-line->map))
             (reduce +))))

(comment
    (solve "resources/test1")
    (solve "resources/test2")

    ((comp game-power
           (partial partition 2 2)
           flatten
           :sets
           input-line->map) "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))