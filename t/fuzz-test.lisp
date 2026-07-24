(in-package :cl-parser-kit/test)

;;; Fuzz coverage of the public tokenizer/parser boundary. cl-weave's IT-FUZZ
;;; (added in cl-weave v0.9.0) generates adversarial inputs on IT-PROPERTY's
;;; generator/shrinking machinery and treats any trial that signals an error
;;; as a failure to shrink and report, complementing the fixed example-based
;;; and property-based (IT-PROPERTY) tests elsewhere in this suite. A trial
;;; passes by running to completion, so every EXPECT below is an extra
;;; invariant on top of "does not signal".

(defparameter %fuzz-source-alphabet
  (concatenate 'string
               "abcXYZ012 +-*/(){}[]<>=!&|,;:.\"'_"
               (string #\Tab)
               (string #\Newline))
  "Letters, digits, and every punctuation character MAKE-BASIC-TOKENIZER-RULES
distinguishes, plus whitespace -- wide enough to exercise identifiers,
numbers, operators, and the UNKNOWN-token fallback, not just well-formed
source.")

(it-fuzz "tokenize-string-never-signals-outside-its-documented-contract"
    ((source (gen-string :min-length 0 :max-length 200
                         :alphabet %fuzz-source-alphabet)))
    (:trials 300 :timeout-per-trial 2)
  (let ((tokens (tokenize-string source (make-tokenizer :rules (%basic-tokenizer-rules)))))
    (expect (vectorp tokens) :to-be-truthy)
    (loop for token across tokens
          do (expect (<= 0 (token-start token) (token-end token) (length source))
                     :to-be-truthy))))

(it-fuzz "parse-tokens-never-signals-outside-run-parser-contract"
    ((tokens (gen-vector
              (gen-member (list (make-token :type :identifier :text "x")
                                (make-token :type :comma :text ",")
                                (make-token :type :number :text "1" :value 1)))
              :min-length 0 :max-length 60)))
    (:trials 300 :timeout-per-trial 2)
  (multiple-value-bind (ok value next failure)
      (parse-tokens (sep-by (type-token :identifier) (type-token :comma)) tokens)
    (declare (ignore value))
    (expect (integerp next) :to-be-truthy)
    (expect (<= 0 next (length tokens)) :to-be-truthy)
    (if ok
        (expect failure :to-be-falsy)
        (expect (cl-parser-kit::parse-failure-p failure) :to-be-truthy))))
