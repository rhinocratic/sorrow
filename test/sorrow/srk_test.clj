(ns sorrow.srk_test
  (:require [clojure.test :refer :all]
            [sorrow.srk :refer :all]))

(def first-50-primes
  [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
   103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197
   199 211 223 227 229])

(deftest test-prime?
  (testing "Primality predicate"
    (is (= first-50-primes (->> (iterate inc 1)
                             (filter #'sorrow.srk/prime?)
                             (take 50))))))

(deftest test-gcd
  (testing "Greatest common divisor"
    (is (= [-9 47 2] (#'sorrow.srk/gcd 240 46)))
    (is (= [47 -9 2] (#'sorrow.srk/gcd 46 240)))))

(deftest test-lcm
  (testing "Least common multiple"
    (is (= 12 (#'sorrow.srk/lcm 4 6)))))

(deftest test-mod-inverse
  (testing "Modular inverse"
    (is (= [1 6 4 3 9 2 8 7 5 10] (map #(#'sorrow.srk/mod-inverse 11 %) (range 1 11))))))

(deftest test-code-length
  (testing "Calculate the length of an encoded word for a given alphabet size and weight parameter"
    (is (= 13 (#'sorrow.srk/code-length 37 0)))))

(deftest test-weight-parameters
  (testing "Calculation of weight parameters (method 1)"
    (is (= [11 0] (#'sorrow.srk/weight-parameters 37 13)))))

(deftest test-weight-sequences
  (testing "Calculation of weight sequences (method 1)"
    (is (= [[12 13 14 15 16 17 18 19 20 21 22 23 24]
            [12 26 5 23 6 28 15 4 32 25 20 17 16]]
           (#'sorrow.srk/weight-sequences 37 13)))))

(deftest test-weighted-sum
  (testing "Weighted sum of a sequence of integers modulo p"
    (is (= 17 (#'sorrow.srk/weighted-sum 37 [1 2 3 4 5 6] [1 2 3 4 5 6])))))

(deftest test-scalev
  (testing "Scale a vector, reducing entries modulo p"
    (is (= [20 12 21] (#'sorrow.srk/scalev 37 5 [78 32 130])))))

(deftest test-minusv
  (testing "Subtract vectors, reducing entries modulo p"
    (is (= [27 2 35] (#'sorrow.srk/minusv 37 [21 8 34] [31 6 36])))))

(deftest test-eliminate-ith-term
  (testing "Eliminate ith term from a pair of simultaneous congruences modulo p"
    (is (= [29 33 4] (#'sorrow.srk/eliminate-ith-term 37 [1 2 3 4] [5 6 7 8] 2)))))

(deftest test-solve-linear-congruence
  (testing "Solve a linear congruence modulo p"
    (is (= 12 (#'sorrow.srk/solve-linear-congruence 37 [27 9])))
    (is (= 35 (#'sorrow.srk/solve-linear-congruence 37 [26 15])))))

(deftest test-solve-simultaneous-congruences
  (testing "Solve simultaneuous congruences modulo p"
    (is (= [12 35] (#'sorrow.srk/solve-simultaneous-congruences 37 [30 9 26] [20 34 2])))))
