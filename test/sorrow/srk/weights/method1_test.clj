(ns sorrow.srk.weights.method1-test
  (:require [clojure.test :refer :all]
            [sorrow.srk.weights.method1 :refer :all]))

(deftest test-code-length
  (testing "Calculate the length of an encoded word for a given alphabet size and weight parameter"
    (is (= 13 (#'sorrow.srk.weights.method1/code-length 37 0)))))

(deftest test-weight-parameters
  (testing "Calculation of weight parameters"
    (is (= {:a 11
            :b 0}
          (#'sorrow.srk.weights.method1/weight-parameters 37 13)))))

(deftest test-weight-scheme
  (testing "Calculation of weight scheme"
    (is (= {:a 11
            :b 0
            :w [12 13 14 15 16 17 18 19 20 21 22 23 24]
            :w' [12 26 5 23 6 28 15 4 32 25 20 17 16]}
          (weight-scheme 37 13)))))
