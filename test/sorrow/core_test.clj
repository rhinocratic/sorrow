(ns sorrow.core-test
  (:require [clojure.test :refer :all]
            [sorrow.core :refer :all]
            [sorrow.weights.method2 :as wm2]))



; (deftest test-encoder
;   (testing "Creation of encoder for correct weight scheme"
;     (let [enc (encoder alphanumeric-upper-case 8)]
;       (is (= "4H9SC5ZC" (enc "4H9SC5"))))))
