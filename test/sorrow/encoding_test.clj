(ns sorrow.encoding-test
  (:require [clojure.test :refer :all]
            [sorrow.encoding :refer :all]
            [sorrow.core :refer [alphanumeric-upper-case]]
            [sorrow.weights.method2 :as wm2]))

(deftest test-checksum-appender
  (testing "Append a two-number checksum to a sequence of numbers"
    (let [ws {:p 37
              :w [4 16 27 34 25 26 30 9]
              :w' [35 4 29 16 5 27 20 34]}
          appender (#'sorrow.encoding/checksum-appender ws)]
      (is (= [4 17 9 28 12 5 35 12] (appender [4 17 9 28 12 5]))))))

(deftest test-encoder-for-weight-scheme
  (testing "Example encoding by method 2"
    (let [ws (wm2/weight-scheme alphanumeric-upper-case 8)
          enc (encoder-for-weight-scheme ws)]
      (is (= "4H9SC5ZC" (enc "4H9SC5"))))))
