(ns sorrow.location.core
   (:require [sorrow.location.method1 :as lm1]
             [sorrow.location.method2 :as lm2]))

(defn- error-locator
  "Create an error locator for the given weight scheme"
  [{:keys [method] :as ws}]
  (if (= method 1)
    (lm1/locator ws)
    (lm2/locator ws)))
