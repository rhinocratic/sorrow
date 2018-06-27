(ns sorrow.encoding-test
  (:require [clojure.test :refer :all]
            [sorrow.encoding :refer :all]
            [sorrow.core :refer [alphanumeric-upper-case]]
            [sorrow.weights.method2 :as wm2]))

(deftest test-checksum-appender
  (testing "Append a two-number checksum to a sequence of positive numbers"
    (let [appender (#'sorrow.encoding/checksum-appender 37 [4 16 27 34 25 26 30 9] [35 4 29 16 5 27 20 34])]
      (is (= [4 17 9 28 12 5 35 12] (appender [4 17 9 28 12 5]))))))

(deftest test-encoding-method-2
  (testing "Example encoding by method 2 (from paper)"
    (let [ws (wm2/weight-scheme 37 8)
          enc (#'sorrow.encoding/encoder-for-weight-scheme alphanumeric-upper-case ws)]
      (is (= "4H9SC5ZC" (enc "4H9SC5"))))))
