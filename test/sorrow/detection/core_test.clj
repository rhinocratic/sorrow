(ns sorrow.detection.core-test
  (:require [clojure.test :refer :all]
            [sorrow.detection.core :refer :all]))

(deftest test-checksum-calculator
  (testing "Calculation of checksums"
    (let [ws {:p 37 :w [4 16 27 34 25 26 30 9] :w' [35 4 29 16 5 27 20 34]}
          calc (#'sorrow.detection.core/checksum-calculator ws)]
      (is (= [0 0] (calc [4 17 9 28 12 5 35 12])))
      (is (= [16 1] (calc [4 17 9 35 12 5 35 12])))
      (is (= [7 6] (calc [4 17 9 28 5 12 35 12]))))))

(deftest test-classify-checksums
  (testing "Classification of checksum pairs"
    (is (= :correct (#'sorrow.detection.core/classify-checksums [0 0])))
    (is (= :uncorrectable (#'sorrow.detection.core/classify-checksums [5 0])))
    (is (= :correctable (#'sorrow.detection.core/classify-checksums [7 6])))))
