(in-package :cl-parser-kit/test)

(it-sequential "pratt-expression-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-expression-tokens*
                                                  *number-plus-operators*)
      (value next)
    (expect next :to-equal 3)
    (expect value :to-equal '(:add 1 2))))

(it-sequential "pratt-postfix-expression-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-postfix-tokens*
                                                  *number-bang-operators*)
      (value next)
    (expect next :to-equal 3)
    (expect value :to-equal '(:fact (:fact 2)))))

(it-sequential "pratt-postfix-binds-tighter-than-infix-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-postfix-precedence-tokens*
                                                  *number-plus-bang-operators*)
      (value next)
    (expect next :to-equal 4)
    (expect value :to-equal '(:add 1 (:fact 2)))))

(it-sequential "pratt-left-associative-infix-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-left-associative-tokens*
                                                  *number-plus-operators*)
      (value next)
    (expect next :to-equal 5)
    (expect value :to-equal '(:add (:add 1 2) 3))))

(it-sequential "pratt-right-associative-infix-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-right-associative-tokens*
                                                  *number-caret-operators*)
      (value next)
    (expect next :to-equal 5)
    (expect value :to-equal '(:pow 2 (:pow 3 4)))))

(it-sequential "pratt-position-starts-from-specified-token-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-position-tokens*
                                                  *number-plus-operators*
                                                  :position 1)
      (value next)
    (expect value :to-equal '(:add 1 2))
    (expect next :to-equal 4)))

(it-sequential "pratt-all-position-consumes-suffix-only-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *pratt-position-tokens*
                                                  *number-plus-operators*
                                                  :position 1)
      (value next)
    (expect value :to-equal '(:add 1 2))
    (expect next :to-equal 4)))

(it-sequential "pratt-min-binding-power-defers-lower-precedence-infix-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt
                                                  *pratt-min-binding-power-tokens*
                                                  *number-plus-star-operators*
                                                  :position 2
                                                  :min-binding-power 11)
      (value next)
    (expect value :to-equal '(:mul 2 3))
    (expect next :to-equal 5)))

(it-sequential "pratt-falls-back-to-token-text-when-type-is-missing-test"
  (assert-pratt-success-values (%run-pratt-parse #'parse-pratt-all
                                                  *textual-number-plus-bang-tokens*
                                                  *textual-number-plus-bang-operators*)
      (value next)
    (expect next :to-equal 4)
    (expect value :to-equal '(:add 1 (:fact 2)))))
