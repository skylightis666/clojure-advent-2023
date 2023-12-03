(ns day-3-task-2.core
    (:require [clojure.set :refer [union]]))

(defn create-2d-arr-from-file [file-name]
    (with-open [rdr (clojure.java.io/reader file-name)]
        (mapv vec (into [] (line-seq rdr)))))

(defn add-borders [arr]
    (let [left-right (mapv
                         (comp
                             #(into [] %)
                             #(concat [\.] % [\.]))
                         arr)
          top-bottom [(into [] (repeat (count (first left-right)) \.))]]
        (into [] (concat top-bottom left-right top-bottom))))

(defn symbol-in-neighborhood [arr i j]
    (for [ii (mapv (partial + i) [-1 0 1])
          jj (mapv (partial + j) [-1 0 1])
          :when (not= [i j] [ii jj])
          :when (= \* (get-in arr [ii jj]))]
        [ii jj]))

(defn clean [arr]
    (mapv #(mapv (fn [param1]
                     (if (#{\. \1 \2 \3 \4 \5 \6 \7 \8 \9 \0 \*} param1)
                         param1
                         \.)) %) arr))

(defn numbers-search [arr]
    (let [len (count (first arr))]
        (loop [i 1
               j 1
               number-acc "0"
               number-and-stars-coords ()
               stars-coords #{}]
            (if (= i len)
                number-and-stars-coords
                (let [curr-elem (get-in arr [i j])]
                    (if (Character/isDigit ^char curr-elem)
                        (recur (+ i (quot j (dec len)))
                               (mod (inc j) len)
                               (str number-acc curr-elem)
                               number-and-stars-coords
                               (into stars-coords (symbol-in-neighborhood arr i j)))
                        (recur (+ i (quot j (dec len)))
                               (mod (inc j) len)
                               "0"
                               (if (empty? stars-coords)
                                   number-and-stars-coords
                                   (conj number-and-stars-coords {:number       (Integer/parseInt number-acc)
                                                                  :stars-coords stars-coords}))
                               (empty stars-coords))
                        ))))))


(defn get-stars [coll]
    (reduce union (map :stars-coords coll)))

(defn set->map [coll]
    (reduce #(assoc %1 %2 []) {} coll))

;(defn find-number-for-star [coll]
;    (let [acc (-> coll
;                  get-stars
;                  set->map)]
;        (reduce #(update %1 %2 conj) coll )))


(defn get-numbers [coll star-coord]
    (filter some? (map #(if ((:stars-coords %) star-coord)
                            (:number %)) coll)))

(comment
    (get-numbers '({:number 598, :stars-coords #{[9 6]}}
                   {:number 755, :stars-coords #{[9 6]}}
                   {:number 617, :stars-coords #{[5 4]}}
                   {:number 35, :stars-coords #{[2 4]}}
                   {:number 467, :stars-coords #{[2 4]}}) [9 6])
    )

(def test1-inp
    (-> "resources/test1"
        create-2d-arr-from-file
        add-borders))

(def ttt
    (->> "resources/test1"
         create-2d-arr-from-file
         add-borders
         clean
         numbers-search))

(comment
    (create-2d-arr-from-file "resources/test1")
    (symbol-in-neighborhood test1-inp 9 5)
    (for [ii (mapv (partial + 1) [-1 0 1])
          jj (mapv (partial + 1) [-1 0 1])
          :when (not= [1 1] [ii jj])]
        [ii jj])
    (numbers-search test1-inp)
    (numbers-search (add-borders (create-2d-arr-from-file "resources/test1withhalf")))
    (-> "resources/test2"
        create-2d-arr-from-file
        first
        count)
    (->> "resources/test1"
         create-2d-arr-from-file
         add-borders
         clean
         numbers-search)

    (let [mmm (-> ttt
                  get-stars
                  set->map)]
        (for [x (get-stars ttt)
              y ttt
              :when ((:stars-coords y) x)
              :let [mmm (update mmm x conj (:number y))]]
            mmm))

    (group-by :stars-coords ttt)

    (symbol-in-neighborhood (add-borders (create-2d-arr-from-file "resources/test2")) 139 130)
    (Integer/parseInt "0123")
    (into (into #{} '([1 2])) '([1 2]))
    (union #{[1 2] [3 4]} #{[3 4] [5 6]})
    (reduce union '(#{[9 6]} #{[9 6]} #{[5 4]} #{[2 4]} #{[2 4]})))


(defn prepare-data [file-name]
    (->> file-name
         create-2d-arr-from-file
         add-borders
         clean
         numbers-search))

(defn solve [file-name]
    (let [coll (prepare-data file-name)
          stars (get-stars coll)]
        (->> stars
             (map (partial get-numbers coll))
             (filter #(= 2 (count %)))
             (map (partial reduce *))
             (reduce +))))

(comment
    (solve "resources/test1")
    (solve "resources/test2"))
