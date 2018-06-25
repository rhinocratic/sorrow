(ns sorrow.srk.encode_test
  (:require [clojure.test :refer :all]
            [sorrow.srk.encode :refer :all]))

(deftest test-code-length
  (testing "Calculate the length of an encoded word for a given alphabet size and weight parameter"
    (is (= 13 (code-length 37 0)))))

(deftest test-weight-parameters
  (testing "Calculation of weight parameters (method 1)"
    (is (= [11 0] (weight-parameters 37 13)))))

(deftest test-weight-sequences
  (testing "Calculation of weight sequences (method 1)"
    (is (= [[12 13 14 15 16 17 18 19 20 21 22 23 24]
            [12 26 5 23 6 28 15 4 32 25 20 17 16]]
           (weight-sequences 37 13)))))

(deftest test-weighted-sum
  (testing "Weighted sum of a sequence of integers modulo p"
    (is (= 17 (weighted-sum 37 [1 2 3 4 5 6] [1 2 3 4 5 6])))))


(deftest test-checksum-appender
  (testing "Append a checksum to a word"
    (let [appender (checksum-appender 37 [4 16 27 34 25 26 30 9] [35 4 29 16 5 27 20 34])]
      (is (= [4 17 9 28 12 5 35 12] (appender [4 17 9 28 12 5]))))))

; (deftest test-encode-word
;   (testing "Encode a word"
;     (is (= "4H9SC5ZC" (#'sorrow.srk/encode-word "4H9SC5")))))
