(ns sorrow.correction.core
   (:require [sorrow.numeric :as n]))

(defn- checksum-calculator
  "Returns a function that accepts a vector of integers and returns a vector of
   two checksums calculated from the given weight scheme."
  [{:keys [p w w']}]
  (fn [nums]
    (mapv #(n/weighted-sum p nums %) [w w'])))

(defn- classify-checksums
  "Return :correct, :uncorrectable or :correctable depending upon the values of
   checksums s1, s2."
  [s1 s2]
  (condp = (count (filter zero? [s1 s2]))
    2 :correct
    1 :uncorrectable
    0 :correctable))

(defn error-corrector
  "Returns an error detector function for the given weight scheme.  The detector
  accepts a vector of integers representing a word, and returns a map containing
  the following:
  - :original   - the original word
  - :correct    - the word after correction (unchanged if correct,
                  absent if uncorrectable)
  - :status     - :correct, :corrected or :uncorrectable
  - :error-type - :transcription or :transposition if :corrected, or
                  :unknown if :uncorrectable
  - :position   - the position at which the error occurred (absent if :correct
                  or :uncorrectable)"
  [{:keys [method] :as ws}]
  (let [calc (checksum-calculator ws)]
    (fn [nums]
      (let [[s1 s2] (calc nums)
            c (classify-checksums s1 s2)
            res {:original nums :status c}]
        (condp = c
          :correct       (merge res {:correct nums})
          :uncorrectable (merge res {:error-type :unknown})
          :correctable   (merge res {}))))))
