(in-package :cl-parser-kit/test)

(it-sequential "pratt-source-tokenizes-and-consumes-all-test"
  (let ((operator-specs `((:prefix :number 0 nil)
                          (:infix :plus 10 ,#'+ 11))))
    (assert-pratt-success-values (%run-pratt-source-parse "1 + 2"
                                                           *pratt-source-plus-literals*
                                                           operator-specs)
        (value next)
      (expect value :to-equal 3)
      (expect next :to-equal 3))))

(it-sequential "pratt-source-passes-position-and-min-binding-power-test"
  (assert-pratt-success-values (%run-pratt-source-parse ", 1 + 2 * 3"
                                                         *pratt-source-position-literals*
                                                         *number-plus-star-operators*
                                                         :position 3
                                                         :min-binding-power 11)
      (value next)
    (expect value :to-equal '(:mul 2 3))
    (expect next :to-equal 6)))
