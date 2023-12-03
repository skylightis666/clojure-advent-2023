(ns day-3-task-1.core)

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
    (not (every? #{\. \1 \2 \3 \4 \5 \6 \7 \8 \9 \0}
                 (map (partial get-in arr)
                      (for [ii (mapv (partial + i) [-1 0 1])
                            jj (mapv (partial + j) [-1 0 1])
                            :when (not= [i j] [ii jj])]
                          [ii jj])))))

(defn numbers-search [arr]
    (let [len (count (first arr))]
        (loop [i 1
               j 1
               number-acc "0"
               number-sum 0
               good-number false]
            (if (= i len)
                number-sum
                (let [curr-elem (get-in arr [i j])]
                    (if (Character/isDigit ^char curr-elem)
                        (recur (+ i (quot j (dec len)))
                               (mod (inc j) len)
                               (str number-acc curr-elem)
                               number-sum
                               (or good-number (symbol-in-neighborhood arr i j)))
                        (recur (+ i (quot j (dec len)))
                               (mod (inc j) len)
                               "0"
                               (if good-number
                                   (+ number-sum (Integer/parseInt (doto number-acc println)))
                                   number-sum)
                               false)))))))

(def test1-inp
    (create-2d-arr-from-file "resources/test1"))

(comment
    (create-2d-arr-from-file "resources/test1")
    (concat [\.] [1 2 3] [\.])
    (add-borders test1-inp)
    (quot 3.5 4)
    (str "asd" \d)
    (symbol-in-neighborhood test1-inp 2 5)
    (for [ii (mapv (partial + 1) [-1 0 1])
          jj (mapv (partial + 1) [-1 0 1])
          :when (not= [1 1] [ii jj])]
        [ii jj])
    (numbers-search (add-borders test1-inp))
    (numbers-search (add-borders (create-2d-arr-from-file "resources/test1withhalf")))
    (-> "resources/test2"
        create-2d-arr-from-file
        first
        count)
    (-> "resources/test2"
        create-2d-arr-from-file
        count)
    (symbol-in-neighborhood (add-borders (create-2d-arr-from-file "resources/test2")) 139 130)
    (Integer/parseInt "0123"))

(defn solve [file-name]
    (-> file-name
        create-2d-arr-from-file
        add-borders
        numbers-search))

(comment
    (solve "resources/test1")
    (solve "resources/test2"))
