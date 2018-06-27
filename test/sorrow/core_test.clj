(ns sorrow.core-test
  (:require [clojure.test :refer :all]
            [sorrow.core :refer :all]
            [sorrow.weights.method2 :as wm2]))

; (deftest test-choose-method
;   (testing "Condition for choosing the method for generating a weight scheme"
;     (is (= [:method1 :method1 :method1 :method2 :method2]
;           (map (partial #'sorrow.core/choose-method 37) (range 6 19 3))))))
