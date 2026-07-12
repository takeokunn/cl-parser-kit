(in-package :cl-parser-kit/test)

(deftest-case pratt-source-tokenizes-and-consumes-all-test
  (let ((operator-specs `((:prefix :number 0 nil)
                          (:infix :plus 10 ,#'+ 11))))
    (assert-pratt-success-values (%run-pratt-source-parse "1 + 2"
                                                           *pratt-source-plus-literals*
                                                           operator-specs)
        (value next)
      (assert-equal 3 value)
      (assert-equal 3 next))))

(deftest-case pratt-source-passes-position-and-min-binding-power-test
  (assert-pratt-success-values (%run-pratt-source-parse ", 1 + 2 * 3"
                                                         *pratt-source-position-literals*
                                                         *number-plus-star-operators*
                                                         :position 3
                                                         :min-binding-power 11)
      (value next)
    (assert-equal '(:mul 2 3) value)
    (assert-equal 6 next)))
