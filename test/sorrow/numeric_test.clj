(ns sorrow.numeric-test
  (:require [clojure.test :refer :all]
            [sorrow.numeric :refer :all]))

(def first-50-primes
  [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
   103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197
   199 211 223 227 229])

(deftest test-prime?
  (testing "Primality predicate"
    (is (= first-50-primes (->> (iterate inc 1)
                             (filter prime?)
                             (take 50))))))

(deftest test-gcd
  (testing "Greatest common divisor"
    (is (= [-9 47 2] (gcd 240 46)))
    (is (= [47 -9 2] (gcd 46 240)))
    (is (= [-8 -17 1] (gcd 36 -17)))))

(deftest test-lcm
  (testing "Least common multiple"
    (is (= 12 (lcm 4 6)))))

(deftest test-mod-inverse
  (testing "Modular inverse"
    (is (= [1 6 4 3 9 2 8 7 5 10] (map #(mod-inverse 11 %) (range 1 11))))))

(deftest test-weighted-sum
  (testing "Weighted sum of a sequence of integers modulo p"
    (is (= 17 (weighted-sum 37 [1 2 3 4 5 6] [1 2 3 4 5 6])))))

(deftest test-scalev
  (testing "Scale a vector, reducing entries modulo p"
    (is (= [20 12 21] (scalev 37 5 [78 32 130])))))

(deftest test-minusv
  (testing "Subtract vectors, reducing entries modulo p"
    (is (= [27 2 35] (minusv 37 [21 8 34] [31 6 36])))))

(deftest test-eliminate-ith-term
  (testing "Eliminate ith term from a pair of simultaneous congruences modulo p"
    (is (= [29 33 4] (eliminate-ith-term 37 [1 2 3 4] [5 6 7 8] 2)))))

(deftest test-linear-congruence-solver
  (testing "Solve a linear congruence modulo p"
    (let [solver (linear-congruence-solver 37)]
      (is (= 12 (solver [27 9])))
      (is (= 35 (solver [26 15]))))))

(deftest test-simultaneous-congruence-solver
  (testing "Solve simultaneous congruences modulo p"
    (let [solver (simultaneous-congruence-solver 37)]
      (is (= [35 12] (solver [30 9 26] [20 34 2]))))))
