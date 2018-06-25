(ns sorrow.srk.weights.method1-test
  (:require [clojure.test :refer :all]
            [sorrow.srk.weights.method1 :refer :all]))

(deftest test-code-length
  (testing "Calculate the length of an encoded word for a given alphabet size and weight parameter"
    (is (= 13 (code-length 37 0)))))

(deftest test-weight-parameters
  (testing "Calculation of weight parameters"
    (is (= [11 0] (weight-parameters 37 13)))))

(deftest test-weight-sequences
  (testing "Calculation of weight sequences"
    (is (= [[12 13 14 15 16 17 18 19 20 21 22 23 24]
            [12 26 5 23 6 28 15 4 32 25 20 17 16]]
           (weight-sequences 37 13)))))
