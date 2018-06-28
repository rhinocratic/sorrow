(ns sorrow.core-test
  (:require [clojure.test :refer :all]
            [sorrow.core :refer :all]
            [sorrow.weights.method2 :as wm2]))

(deftest test-encoder
  (testing "Creation of encoder (method 1)"
    (let [enc (encoder alphanumeric-upper-case 8)]
      (is (= "4H9SC510" (enc "4H9SC5"))))))

(deftest test-corrector
  (testing "Creation of corrector (method 1)"
    (let [cor (corrector alphanumeric-upper-case 8)]
      (is (= "4H9SC510" (:corrected (cor "4H9ZC510"))))
      (is (= "4H9SC510" (:corrected (cor "5H9SC510"))))
      (is (= "4H9SC510" (:corrected (cor "4H9SC512"))))
      (is (= "4H9SC510" (:corrected (cor "49HSC510"))))
      (is (= "4H9SC510" (:corrected (cor "H49SC510"))))
      (is (= "4H9SC510" (:corrected (cor "4H9SC501"))))))
  (testing "Creation of corrector (method 2)"
    (let [cor (corrector alphanumeric-upper-case 15)]
      (is (= "4H9SC610DQ7R3FC" (:corrected (cor "4H9SC630DQ7R3FC"))))
      (is (= "4H9SC610DQ7R3FC" (:corrected (cor "2H9SC610DQ7R3FC"))))
      (is (= "4H9SC610DQ7R3FC" (:corrected (cor "4H9SC610DQ7R3FG"))))
      (is (= "4H9SC610DQ7R3FC" (:corrected (cor "4HS9C610DQ7R3FC"))))
      (is (= "4H9SC610DQ7R3FC" (:corrected (cor "H49SC610DQ7R3FC"))))
      (is (= "4H9SC610DQ7R3FC" (:corrected (cor "4H9SC610DQ7R3CF")))))))
