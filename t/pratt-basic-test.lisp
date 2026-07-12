(in-package :cl-parser-kit/test)

(deftest-case pratt-expression-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-expression-tokens*
                                                  *number-plus-operators*)
      (value next)
    (assert-equal 3 next)
    (assert-equal '(:add 1 2) value)))

(deftest-case pratt-postfix-expression-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-postfix-tokens*
                                                  *number-bang-operators*)
      (value next)
    (assert-equal 3 next)
    (assert-equal '(:fact (:fact 2)) value)))

(deftest-case pratt-postfix-binds-tighter-than-infix-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-postfix-precedence-tokens*
                                                  *number-plus-bang-operators*)
      (value next)
    (assert-equal 4 next)
    (assert-equal '(:add 1 (:fact 2)) value)))

(deftest-case pratt-left-associative-infix-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-left-associative-tokens*
                                                  *number-plus-operators*)
      (value next)
    (assert-equal 5 next)
    (assert-equal '(:add (:add 1 2) 3) value)))

(deftest-case pratt-right-associative-infix-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-right-associative-tokens*
                                                  *number-caret-operators*)
      (value next)
    (assert-equal 5 next)
    (assert-equal '(:pow 2 (:pow 3 4)) value)))

(deftest-case pratt-position-starts-from-specified-token-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-position-tokens*
                                                  *number-plus-operators*
                                                  :position 1)
      (value next)
    (assert-equal 3 value)
    (assert-equal 4 next)))

(deftest-case pratt-all-position-consumes-suffix-only-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-position-tokens*
                                                  *number-plus-operators*
                                                  :position 1)
      (value next)
    (assert-equal 3 value)
    (assert-equal 4 next)))

(deftest-case pratt-min-binding-power-defers-lower-precedence-infix-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-min-binding-power-tokens*
                                                  *number-plus-star-operators*
                                                  :position 2
                                                  :min-binding-power 11)
      (value next)
    (assert-equal '(:mul 2 3) value)
    (assert-equal 5 next)))

(deftest-case pratt-falls-back-to-token-text-when-type-is-missing-test
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *textual-number-plus-bang-tokens*
                                                  *textual-number-plus-bang-operators*)
      (value next)
    (assert-equal 4 next)
    (assert-equal '(:add 1 (:fact 2)) value)))
