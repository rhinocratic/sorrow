(ns sorrow.encoding-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [is]])
            [sorrow.encoding :refer :all]
            [sorrow.core :refer [alphanumeric-upper-case]]
            [sorrow.weights.method1 :as wm1]
            [sorrow.weights.method2 :as wm2]))

(deftest test-checksum-appender
  (testing "Append checksum to a sequence of numbers - weight sequence for method 1"
    (let [ws {:p 37
              :w [12 13 14 15 16 17 18 19]
              :w' [12 26 5 23 6 28 15 4]}
          appender (#'sorrow.encoding/checksum-appender ws)]
      (is (= [4 17 9 28 12 5 1 0] (appender [4 17 9 28 12 5])))))
  (testing "Append checksum to a sequence of numbers - weight sequence for method 2"
    (let [ws {:p 37
              :w [4 16 27 34 25 26 30 9]
              :w' [35 4 29 16 5 27 20 34]}
          appender (#'sorrow.encoding/checksum-appender ws)]
      (is (= [4 17 9 28 12 5 35 12] (appender [4 17 9 28 12 5]))))))

(deftest test-encoder
  (testing "Example encoding by method 1"
    (let [ws (wm1/weight-scheme alphanumeric-upper-case 8)
          enc (encoder ws)]
      (is (= "4H9SC510" (enc "4H9SC5")))))
  (testing "Example encoding by method 2"
    (let [ws (wm2/weight-scheme alphanumeric-upper-case 8)
          enc (encoder ws)]
      (is (= "4H9SC5ZC" (enc "4H9SC5"))))))
