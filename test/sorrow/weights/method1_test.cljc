(ns sorrow.weights.method1-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest testing is]])
            [sorrow.weights.method1 :refer [weight-scheme]]))

(deftest test-weight-scheme
  (testing "Calculation of weight scheme"
    (is (= {:p 37
            :n 13
            :a 11
            :b 0
            :w [12 13 14 15 16 17 18 19 20 21 22 23 24]
            :w' [12 26 5 23 6 28 15 4 32 25 20 17 16]
            :alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*"
            :method 1}
          (weight-scheme "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*" 13)))
    (is (= {:p 37
            :n 8
            :a 11
            :b 0
            :w [12 13 14 15 16 17 18 19]
            :w' [12 26 5 23 6 28 15 4]
            :alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*"
            :method 1}
          (weight-scheme "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*" 8)))))
