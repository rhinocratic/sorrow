(ns sorrow.correction
   (:require [sorrow.numeric :as n]
             [sorrow.translation :as t]
             [sorrow.location :as l]))

(defn- checksum-calculator
  "Returns a function that accepts a vector of integers and returns a vector of
   two checksums calculated from the given weight scheme."
  [{:keys [p w w']}]
  (fn [word]
    (mapv #(n/weighted-sum p word %) [w w'])))

(defn- classify-checksums
  "Return :correct, :uncorrectable or :correctable depending upon the values of
   checksums s1, s2."
  [s1 s2]
  (condp = (count (filter zero? [s1 s2]))
    2 :correct
    1 :uncorrectable
    0 :correctable))

; (defn classify-error
;   "Classifies an error as :transcription, :transposition or :uncorrectable based
;    upon the values of error position indicators ep1, ep2."
;   [n [ep1 ep2]]
;   (cond
;     (<= 0 ep1 (dec n))       :transcription
;     (and (int? ep2)
;          (<= 0 ep2 (- n 2))) :transposition
;     :else                    :uncorrectable))

(defn correct-transcription-error
  "Correct a single transcription error"
  [word pos size]
  (update word pos #(- % size)))

(defn correct-transposition-error
  "Correct a transposition of adjacent characters"
  [word pos]
  (concat
    (take pos word)
    [(word (inc pos)) (word pos)]
    (drop (inc pos) word)))

(defn- correct-error
  "Correct an error if possible, returning a map for merging with the output
   of the corrector."
  [{:keys [status original error-type error-pos error-size] :as det}]
  (if (= status :uncorrectable)
    (select-keys det [:original :status])
    (merge (dissoc det :error-size)
      {:correct
       (condp = error-type
         :transcription (correct-transcription-error original error-pos error-size)
         :transposition (correct-transposition-error original error-pos))})))

; (defn- error-detector
;   "Create an error detector for the given weight scheme"
;   [{:keys [method]}]
;   (if (= method 1)
;     (cm1/detector ws)
;     (cm2/detector ws)))

(defn- error-corrector
  "Create an error corrector for the given weight scheme"
  [{:keys [p n]}])

(defn- update-if-exists
  "Update a map entry if it exists"
  [m k f]
  (if (contains? m k)
    (update m k f)
    m))

(defn corrector
  "Returns a validator/corrector function for the given weight scheme.  The
  function accepts a vector of integers representing a word, and returns a map
  containing the following:
    :original    - the uncorrected word, as supplied to the corrector
    :status      - :correct, :corrected or :uncorrectable
    :correct     - the corrected word (absent if status is :uncorrectable)
    :error-type  - :transcription or :transposition if :status is :corrected
    :error-pos   - position of the error in the word if :status is :corrected"
  [{:keys [p n alphabet method] :as ws}]
  (let [check (checksum-calculator ws)
        ; detect-error (error-detector ws)
        correct-error (error-corrector ws)
        to-ints (t/str->ints alphabet)
        to-str (t/ints->str alphabet)]
    (fn [w]
      {:pre [(every? (set alphabet) w) (= n (count w))]}
      (-> w
        to-ints
        ; detect-error
        correct-error
        (update :original to-str)
        (update-if-exists :correct to-str)))))

    ; (fn [word]
    ;   (let [[s1 s2] (calc word)
    ;         status (classify-checksums s1 s2)
    ;         res {:original word :status status}]
    ;     (condp = status
    ;       :correct       (merge res {:correct word})
    ;       :uncorrectable (merge res {:error-type :unknown})
    ;       :correctable   (merge res (-> (det word)
    ;                                   (partial correct-error p))))))))
