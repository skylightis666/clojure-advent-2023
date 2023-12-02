(ns day-2-task-2.core-test
  (:require [clojure.test :refer :all]
            [day-2-task-2.core :refer :all]))

(deftest a-test
    (is (= (solve "resources/test1")
           2286))

    (is (= (solve "resources/test2")
           65371)))
