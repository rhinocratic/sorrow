(ns sorrow.srk.weights.method2-test
  (:require [clojure.test :refer :all]
            [sorrow.srk.weights.method2 :refer :all]))

(deftest test-diff-coprime?
  (testing "Predicate to determine if x is coprime to b - a"
    (is (true? (#'sorrow.srk.weights.method2/diff-coprime? 2 [2 5])))
    (is (false? (#'sorrow.srk.weights.method2/diff-coprime? 2 [2 6])))))

(deftest test-gcd-lte-2?
  (testing "Predicate to determine if gcd(x,y) <= 2"
    (is (true? (#'sorrow.srk.weights.method2/gcd-lte-2? 3 5)))
    (is (true? (#'sorrow.srk.weights.method2/gcd-lte-2? 2 6)))
    (is (false? (#'sorrow.srk.weights.method2/gcd-lte-2? 12 6)))))

(deftest test-powers-of-n
  (testing "Generation of distinct powers of n modulo p"
    (is (= {1 2, 2 4, 3 8, 4 5, 5 10, 6 9, 7 7, 8 3, 9 6, 10 1}
          (#'sorrow.srk.weights.method2/powers-of-n 11 2)))))

(deftest test-distinct-pairs
  (testing "Generation of distinct unequal pairs from a collection of distinct elements"
    (is (= [[1 2] [1 3] [1 4] [2 3] [2 4] [3 4]]
          (#'sorrow.srk.weights.method2/distinct-pairs [1 2 3 4])))))

(deftest test-solution-predicate
  (testing "Predicate for detecting pairs of solutions for weight parameters"
    (let [pred (#'sorrow.srk.weights.method2/solution-predicate 37)]
      (is (true? (pred [17 34])))
      (is (true? (pred [34 17])))
      (is (true? (pred [2 19])))
      (is (false? (pred [3 34]))))))

(deftest test-candidate-pairs
  (testing "Generation of solution pairs for weight parameters"
    (is (= [[2 19] [14 19] [17 22] [17 34]]
          (#'sorrow.srk.weights.method2/candidate-pairs 37)))))

(deftest test-weight-parameters
  (testing "Selection of first candidate solution for weight parameters"
    (is (= [2 19] (#'sorrow.srk.weights.method2/weight-parameters 37)))))

(deftest test-gen-sequence
  (testing "Generation of sequences of every mth power of 2 modulo p"
    (is (= [4 16 27 34 25 26 30 9 36 33 21 10 3 12 11 7 28 1]
          (#'sorrow.srk.weights.method2/gen-sequence 37 18 2)))))

(deftest test-weight-sequences
  (testing "Generation of weight sequences for given parameters"
    (is (= [[4 16 27 34 25 26 30 9 36 33 21 10 3 12 11 7 28 1]
            [35 4 29 16 5 27 20 34 6 25 24 26 22 30 14 9 19 36]]
          (#'sorrow.srk.weights.method2/weight-sequences 37 18 [2 19])))
    (is (= [[30 12 27 33 28 26 3 16 36 7 25 10 4 9 11 34 21 1]
            [35 4 29 16 5 27 20 34 6 25 24 26 22 30 14 9 19 36]]
          (#'sorrow.srk.weights.method2/weight-sequences 37 18 [14 19])))))

(deftest test-weight-scheme
  (testing "Creation of weight scheme"
    (is (= {:method 2
            :a 2
            :b 19
            :w [4 16 27 34 25 26 30 9 36 33 21 10 3 12 11 7 28 1]
            :w' [35 4 29 16 5 27 20 34 6 25 24 26 22 30 14 9 19 36]}
          (weight-scheme 37 18)))
    (is (= {:method 2
            :a 2
            :b 19
            :w [4 16 27 34 25 26 30 9]
            :w' [35 4 29 16 5 27 20 34]}
          (weight-scheme 37 8)))))
