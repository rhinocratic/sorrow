(ns sorrow.correction-test
  (:require [clojure.test :refer :all]
            [sorrow.correction :refer :all]))

(deftest test-checksum-calculator
  (testing "Calculation of checksums - weight scheme for method 1"
    (let [ws {:p 37 :w [12 13 14 15 16 17 18 19] :w' [12 26 5 23 6 28 15 4]}
          calc (#'sorrow.correction/checksum-calculator ws)]
      (is (= [0 0] (calc [4 17 9 28 12 5 1 0])))
      (is (= [19 4] (calc [4 17 9 28 12 5 1 1])))
      (is (= [1 26] (calc [4 17 9 28 12 5 0 1])))))
  (testing "Calculation of checksums - weight scheme for method 2"
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

(deftest test-error-classifier
  (testing "Classification of potentially correctable errors")
  (let [ec (#'sorrow.correction/error-classifier {:n 8})]
    (is (= :transcription (ec 5 32)))
    (is (= :transposition (ec 12 4)))
    (is (= :uncorrectable (ec 11 12)))))

(deftest test-correct-transcription-error
  (testing "Correction of transcription errors"
    (is (= [1 2 3 4] (#'sorrow.correction/correct-transcription-error [1 2 42 4] 2 39)))))

(deftest test-correct-transposition-error
  (testing "Correction of transposition errors"
    (is (= [1 2 3 4] (#'sorrow.correction/correct-transposition-error [2 1 3 4] 0)))
    (is (= [1 2 3 4] (#'sorrow.correction/correct-transposition-error [1 3 2 4] 1)))
    (is (= [1 2 3 4] (#'sorrow.correction/correct-transposition-error [1 2 4 3] 2)))))
