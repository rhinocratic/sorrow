(ns sorrow.location-test
  (:require [clojure.test :refer :all]
            [sorrow.location :refer :all]))

(deftest test-method1-locator
  (testing "Error location by method 1"
    (let [ws {:p 37 :n 8}
          loc (#'sorrow.location/method1-locator ws)])))
