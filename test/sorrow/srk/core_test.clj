(ns sorrow.srk.core-test
  (:require [clojure.test :refer :all]
            [sorrow.srk.core :refer :all]))

(deftest test-alphabet?
  (testing "Predicate for valid alphabet"
    (is (true? (#'sorrow.srk.core/alphabet? "abcde")))
    (is (true?  (#'sorrow.srk.core/alphabet? alphanumeric-upper-case)))
    (is (true?  (#'sorrow.srk.core/alphabet? alphanumeric-mixed-case)))
    (is (false?  (#'sorrow.srk.core/alphabet? :abcd)))
    (is (false?  (#'sorrow.srk.core/alphabet? "abcdd")))
    (is (false?  (#'sorrow.srk.core/alphabet? "abcd")))))

(deftest test-choose-method
  (testing "Condition for choosing the method for generating a weight scheme"
    (is (= [:error :method1 :method1 :method1 :method2 :error]
          (map (partial #'sorrow.srk.core/choose-method 37) (range 1 22 4))))))
