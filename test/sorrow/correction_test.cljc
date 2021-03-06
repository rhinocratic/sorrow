(ns sorrow.correction-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [deftest testing is]])
            [sorrow.correction :refer [corrector]]))

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

(deftest test-checksum-status
  (testing "Classification of checksum pairs according to status"
    (is (= :correct (#'sorrow.correction/checksum-status 0 0)))
    (is (= :uncorrectable (#'sorrow.correction/checksum-status 5 0)))
    (is (= :correctable (#'sorrow.correction/checksum-status 7 6)))))

(deftest test-error-classifier
  (testing "Classification of potentially correctable errors")
  (let [ec (#'sorrow.correction/error-classifier {:n 8})]
    (is (= :transcription (ec 5 32)))
    (is (= :transposition (ec 12 4)))
    (is (= :uncorrectable (ec 11 12)))))

(deftest test-error-locator
  (testing "Creation of error locator - method 1"
    (let [ws {:p 37 :n 8 :a 11 :b 0 :method 1}
          loc (#'sorrow.correction/error-locator ws)]
      (is (= {:checksums [19 4] :error-pos 7 :error-size 1 :error-type :transcription}
            (loc {:checksums [19 4]})))
      (is (= {:checksums [1 26] :error-pos 6 :error-size 0 :error-type :transposition}
            (loc {:checksums [1 26]})))))
  (testing "Creation of error locator - method 2"
    (let [ws {:p 37 :n 8 :a 2 :b 19 :method 2}
          loc (#'sorrow.correction/error-locator ws)]
      (is (= {:checksums [16 1] :error-pos 3 :error-size 7 :error-type :transcription}
            (loc {:checksums [16 1]})))
      (is (= {:checksums [7 6] :error-pos 4 :error-size 0 :error-type :transposition}
            (loc {:checksums [7 6]}))))))

(deftest test-correct
  (let [corr #'sorrow.correction/correct]
    (testing "Correction of transcription errors"
      (is (= [1 2 3 4] (:correct (corr {:nums [1 2 35 4] :error-pos 2 :error-size 32 :error-type :transcription :p 37})))))
    (testing "Correction of transposition errors"
      (is (= [1 2 3 4] (:correct (corr {:nums [2 1 3 4] :error-pos 0 :error-type :transposition :p 37}))))
      (is (= [1 2 3 4] (:correct (corr {:nums [1 3 2 4] :error-pos 1 :error-type :transposition :p 37}))))
      (is (= [1 2 3 4] (:correct (corr {:nums [1 2 4 3] :error-pos 2 :error-type :transposition :p 37})))))
    (testing "Handling of uncorrectable errors"
      (is (= {:status :uncorrectable :original "flib"}
            (corr {:error-pos 42 :error-type :uncorrectable :original "flib"}))))))

(deftest test-validator
  (testing "Classification of words according to their checksums"
    (let [ws {:p 37
              :w [4 16 27 34 25 26 30 9]
              :w' [35 4 29 16 5 27 20 34]
              :alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*"}
          classify (#'sorrow.correction/validator ws)]
      (is (= {:original "4H9SC5ZC"
              :nums [4 17 9 28 12 5 35 12]
              :status :correct
              :checksums [0 0]
              :p 37}
            (classify "4H9SC5ZC")))
      (is (= {:original "4H9ZC5ZC"
              :nums [4 17 9 35 12 5 35 12]
              :status :correctable
              :checksums [16 1]
              :p 37}
            (classify "4H9ZC5ZC")))
      (is (= {:original "4H9S5CZC"
              :nums [4 17 9 28 5 12 35 12]
              :status :correctable
              :checksums [7 6]
              :p 37}
            (classify "4H9S5CZC"))))))

(deftest test-corrector
    (testing "Correction - method 1"
      (let [ws {:p 37
                :n 8
                :a 11
                :b 0
                :w [12 13 14 15 16 17 18 19]
                :w' [12 26 5 23 6 28 15 4]
                :alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*"
                :method 1}
            correct (corrector ws)]
        (is (= {:original "4H9ZC510"
                :status :corrected
                :correct "4H9SC510"
                :error-pos 3
                :error-type :transcription}
              (correct "4H9ZC510")))))
  (testing "Correction - method 2"
    (let [ws {:p 37
              :n 8
              :a 2
              :b 19
              :w [4 16 27 34 25 26 30 9]
              :w' [35 4 29 16 5 27 20 34]
              :alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ*"
              :method 2}
          correct (corrector ws)]
      (is (= {:original "4H9ZC5ZC"
              :status :corrected
              :correct "4H9SC5ZC"
              :error-pos 3
              :error-type :transcription}
            (correct "4H9ZC5ZC"))))))
