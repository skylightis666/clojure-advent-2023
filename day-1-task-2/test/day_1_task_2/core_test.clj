(ns day-1-task-2.core-test
  (:require [clojure.test :refer :all]
            [day-1-task-2.core :refer :all]))

(deftest a-test
    (is (= (solve "resources/test1")
           281))

    (is (= (solve "resources/test2")
           54418)))
