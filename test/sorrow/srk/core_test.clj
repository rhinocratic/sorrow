(ns sorrow.srk.core-test
  (:require [clojure.test :refer :all]
            [sorrow.srk.core :refer :all]
            [sorrow.srk.weights.method2 :as wm2]))

(deftest test-str->ints
  (testing "Conversion from strings to vectors of integers"
    (let [conv (#'sorrow.srk.core/str->ints "abcdef")]
      (is (= [3 4 0 3 1 4 4 5] (conv "deadbeef"))))))

(deftest test-ints->str
  (testing "Conversion from vectors of integers to strings"
    (let [conv (#'sorrow.srk.core/ints->str "abcdef")]
      (is (= "deadbeef" (conv [3 4 0 3 1 4 4 5]))))))

; (deftest test-choose-method
;   (testing "Condition for choosing the method for generating a weight scheme"
;     (is (= [:method1 :method1 :method1 :method2 :method2]
;           (map (partial #'sorrow.srk.core/choose-method 37) (range 6 19 3))))))

(deftest test-checksum-appender
  (testing "Append a two-number checksum to a sequence of positive numbers"
    (let [appender (#'sorrow.srk.core/checksum-appender 37 [4 16 27 34 25 26 30 9] [35 4 29 16 5 27 20 34])]
      (is (= [4 17 9 28 12 5 35 12] (appender [4 17 9 28 12 5]))))))

(deftest test-encoding-method-2
  (testing "Example encoding by method 2 (from paper)"
    (let [ws (wm2/weight-scheme 37 8)
          enc (#'sorrow.srk.core/encoder-for-weight-scheme alphanumeric-upper-case 8 ws)]
      (= "4H9SC5Zc" (enc "4H9SC5")))))
