(ns sorrow.location-test
  (:require [clojure.test :refer :all]
            [sorrow.location :refer :all]))

(deftest test-error-locator
  (testing "Error location by method 1"
    (let [ws {:p 37 :n 8 :a 11 :b 0 :method 1}
          loc (#'sorrow.location/error-locator ws)]
      (is (= [7 34 36] (loc 19 4)))
      (is (= [25 6 0] (loc 1 26)))))
  (testing "Error location by method 2"
    (let [ws {:p 37 :n 8 :a 2 :b 19 :method 2}
          loc (#'sorrow.location/error-locator ws)]
      (is (= [4 22 7] (loc 16 1)))
      (is (= [23 5 0] (loc 7 6))))))
