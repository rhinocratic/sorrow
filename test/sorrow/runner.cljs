(ns sorrow.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [sorrow.core-test]
              [sorrow.correction-test]
              [sorrow.encoding-test]
              [sorrow.location-test]
              [sorrow.numeric-test]
              [sorrow.translation-test]
              [sorrow.weights.method1-test]
              [sorrow.weights.method2-test]))

(doo-tests 'sorrow.core-test
           'sorrow.correction-test
           'sorrow.encoding-test
           'sorrow.location-test
           'sorrow.numeric-test
           'sorrow.translation-test
           'sorrow.weights.method1-test
           'sorrow.weights.method2-test)
