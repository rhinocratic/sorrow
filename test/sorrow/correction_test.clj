(ns sorrow.correction-test
  (:require [clojure.test :refer :all]
            [sorrow.correction :refer :all]))

(deftest test-checksum-calculator
  (testing "Calculation of checksums"
    (let [ws {:p 37 :w [4 16 27 34 25 26 30 9] :w' [35 4 29 16 5 27 20 34]}
          calc (#'sorrow.correction/checksum-calculator ws)]
      (is (= [0 0] (calc [4 17 9 28 12 5 35 12])))
      (is (= [16 1] (calc [4 17 9 35 12 5 35 12])))
      (is (= [7 6] (calc [4 17 9 28 5 12 35 12]))))))

(deftest test-classify-checksums
  (testing "Classification of checksum pairs"
    (is (= :correct (#'sorrow.correction/classify-checksums 0 0)))
    (is (= :uncorrectable (#'sorrow.correction/classify-checksums 5 0)))
    (is (= :correctable (#'sorrow.correction/classify-checksums 7 6)))))
