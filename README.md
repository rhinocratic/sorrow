# sorrow

[![Build Status](https://travis-ci.org/rhinocratic/sorrow.svg?branch=master)](https://travis-ci.org/rhinocratic/sorrow)
[![License](https://img.shields.io/badge/License-EPL%201.0-red.svg)](https://opensource.org/licenses/EPL-1.0)

A Clojure/ClojureScript implementation of an error-correcting coding scheme for alphanumeric data, as described by A.S. Sethi, V. Rajaraman and P.S. Kenjale in their 1977 [paper](https://vdocuments.site/download/an-error-correcting-coding-scheme-for-alphanumeric-data).

The encoding appends two check characters to an alphanumeric string in order to enable the detection
and correction of the following types of error:

- a single error in transcription of a character *OR*
- a single transposition of adjacent characters

For an alphabet of (prime) size p, the maximum length of an encoded word will be (p - 1)/2.  The
check characters are calculated by one of two different methods according to the desired length
n of the encoded word.  For n <= ⌊(p + 2)/3⌋, the simpler of the two methods described in the paper
will be used, the more complex second method being reserved for cases when longer words are required.

## Encoding

<!-- For Leiningen, add the following to the ```dependencies``` section of your project.clj:
```clojure
[sorrow "0.1.0"]
``` -->

To create an encoder for an alphabet and a desired encoded word length n:
```clojure
(require '[sorrow.core :refer [encoder corrector alphanumeric-upper-case]])

(def enc (encoder alphanumeric-upper-case 8))
```

The cardinality of the alphabet must be prime.  The returned encoding function will accept words of length n-2 formed from the alphabet and
append the two check characters:
```clojure
(enc "ABC123")

;; => "ABC1235K"
```

Sorrow pre-defines a couple of alphabets:

- ```alphanumeric-upper-case``` - The digits 0-9, letters A-Z plus * to make the cardinality prime (37 characters)
- ```alphanumeric-mixed-case``` - The digits 0-9, letters A-Za-z minus capital 'O' (61 characters)

However, any alphabet of prime size and with distinct characters may be supplied as a string:
```clojure
(def my-alphabet "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ~@#!*")
(def enc (encoder my-alphabet 8))
```

## Validation/correction

To obtain a corrector for encoded words of length n:
```clojure
(def cor (corrector alphanumeric-upper-case 8))
```

For an encoded string of length n, the corrector function will return a map containing the following:

- ```:status```      - ```:correct```, ```:corrected``` or ```:uncorrectable```
- ```:original```    - the uncorrected word, as supplied to the corrector
- ```:correct```     - the corrected word (absent if ```:status``` = ```:uncorrectable```, same as ```:original``` if ```:status``` = ```:correct```)
- ```:error-type```  - ```:transcription``` or ```:transposition``` if ```:status``` = ```:corrected```
- ```:error-pos```   - position of the error in the word if ```:status``` = ```:corrected```

For example:

```clojure
(cor "4H9SC512")

;; => {:status :corrected, :original "4H9SC512", :correct "4H9SC510", :error-type :transcription, :error-pos 7}
```

## Running the project's tests
```
$ clojure -T:build test
```

## License

Copyright © 2023 Andrew Baxter

Distributed under the Eclipse Public License version 1.0.
