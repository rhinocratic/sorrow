(ns sorrow.location-test
  (:require [clojure.test :refer :all]
            [sorrow.location :refer :all]))

(deftest test-method2-locator
  (testing "Error location by method 2"
    (let [ws {:p 37 :n 8 :a 2 :b 19}
          loc (#'sorrow.location/method2-locator ws)]
      (is (= [4 22 7] (loc 16 1)))
      (is (= [23 5 0] (loc 7 6))))))
