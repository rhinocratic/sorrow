(ns sorrow.location-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest testing is]])
            [sorrow.location :refer [error-locator]]))

(deftest test-error-locator
  (testing "Error location by method 1"
    (let [ws {:p 37 :n 8 :a 11 :b 0 :method 1}
          loc (error-locator ws)]
      (is (= [7 -3 1] (loc 19 4)))
      (is (= [25 6 0] (loc 1 26)))))
  (testing "Error location by method 2"
    (let [ws {:p 37 :n 8 :a 2 :b 19 :method 2}
          loc (error-locator ws)]
      (is (= [3 21 7] (loc 16 1)))
      (is (= [22 4 0] (loc 7 6))))))
