(ns sorrow.sethi_test
  (:require [clojure.test :refer :all]
            [sorrow.sethi :refer :all]))

(def first-50-primes
  [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
   103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197
   199 211 223 227 229])

(deftest test-prime?
  (testing "Primality predicate"
    (is (= first-50-primes (->> (iterate inc 1)
                             (filter #'sorrow.sethi/prime?)
                             (take 50))))))

(deftest test-gcd
  (testing "Greatest common divisor"
    (is (= [-9 47 2] (#'sorrow.sethi/gcd 240 46)))
    (is (= [47 -9 2] (#'sorrow.sethi/gcd 46 240)))))

(deftest test-mod-inverse
  (testing "Modular inverse"
    (is (= [1 6 4 3 9 2 8 7 5 10] (map #(#'sorrow.sethi/mod-inverse % 11) (range 1 11))))))

(deftest test-code-length
  (testing "Calculate the length of an encoded word for a given alphabet size and weight parameter"
    (is (= 13 (#'sorrow.sethi/code-length 37 0)))))

(deftest test-weight-parameters
  (testing "Calculation of weight parameters (method 1)"
    (is (= [11 0] (#'sorrow.sethi/weight-parameters 13 37)))))

(deftest test-weight-sequences
  (testing "Calculation of weight sequences (method 1)"
    (is (= [[12 13 14 15 16 17 18 19 20 21 22 23 24]
            [12 26 5 23 6 28 15 4 32 25 20 17 16]]
           (#'sorrow.sethi/weight-sequences 13 37)))))
