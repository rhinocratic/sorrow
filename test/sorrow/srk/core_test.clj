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
