(ns sorrow.correction.method2)

(defn corrector
  "Returns an error detector function for the given weight scheme, accepting
   integer vectors of length n and returning a map containing:
     :error-type - :transcription or :transposition (if correctable)
     :error-pos  - position of the error (if corrected)
     :error-size - magnitude of the error (only for transcription errors)
     :status     - only present if the value is :uncorrectable"
  [ws])
