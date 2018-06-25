(ns sorrow.srk.encode-test
  (:require [clojure.test :refer :all]
            [sorrow.srk.encode :refer :all]))

(deftest test-checksum-appender
  (testing "Append a checksum to a word"
    (let [appender (checksum-appender 37 [4 16 27 34 25 26 30 9] [35 4 29 16 5 27 20 34])]
      (is (= [4 17 9 28 12 5 35 12] (appender [4 17 9 28 12 5]))))))

; (deftest test-encode-word
;   (testing "Encode a word"
;     (is (= "4H9SC5ZC" (#'sorrow.srk/encode-word "4H9SC5")))))
