(ns sorrow.srk.weights.method1-test
  (:require [clojure.test :refer :all]
            [sorrow.srk.weights.method1 :refer :all]))

(deftest test-weight-scheme
  (testing "Calculation of weight scheme"
    (is (= {:method 1
            :a 11
            :b 0
            :w [12 13 14 15 16 17 18 19 20 21 22 23 24]
            :w' [12 26 5 23 6 28 15 4 32 25 20 17 16]}
          (weight-scheme 37 13)))))
