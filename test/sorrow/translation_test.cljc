(ns sorrow.translation-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :refer-macros [is]])
            [sorrow.translation :refer :all]))

(deftest test-str->ints
  (testing "Conversion from strings to vectors of integers"
    (let [conv (str->ints "abcdef")]
      (is (= [3 4 0 3 1 4 4 5] (conv "deadbeef"))))))

(deftest test-ints->str
  (testing "Conversion from vectors of integers to strings"
    (let [conv (ints->str "abcdef")]
      (is (= "deadbeef" (conv [3 4 0 3 1 4 4 5]))))))
