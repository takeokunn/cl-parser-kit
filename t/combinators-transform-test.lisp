(in-package :cl-parser-kit/test)

(deftest-case combinator-bind-parser-preserves-failure-position-test
  (let* ((parser (bind-parser (type-token :identifier)
                              (lambda (_token)
                                (declare (ignore _token))
                                (type-token :number))))
         (tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :plus :text "+"))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (assert-false ok)
      (assert-false value)
      (assert-equal 1 next)
      (assert-equal 1 (parse-failure-position failure))
      (assert-equal :number (parse-failure-expected failure)))))

(deftest-case combinator-bind-parser-preserves-success-diagnostics-test
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let* ((diagnostic-parser (opt (lookahead (seq (type-token :identifier)
                                                   (end-of-input)))))
           (parser (bind-parser diagnostic-parser
                                (lambda (_value)
                                  (declare (ignore _value))
                                  diagnostic-parser))))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (assert-true ok)
        (assert-false value)
        (assert-equal 0 next)
        (assert-equal 2 (length diagnostics))
        (dolist (diagnostic diagnostics)
          (%assert-rendered-diagnostic-contains diagnostic
                                                "Unexpected trailing token"
                                                "1:4-1:5"))))))

(deftest-case combinator-map-parser-preserves-failure-position-test
  (let* ((parser (map-parser (seq (type-token :identifier)
                                  (type-token :number))
                             #'identity))
         (tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :plus :text "+"))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (assert-false ok)
      (assert-false value)
      (assert-equal 1 next)
      (assert-equal 1 (parse-failure-position failure))
      (assert-equal :number (parse-failure-expected failure)))))

(deftest-case combinator-map-parser-preserves-success-diagnostics-test
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let ((parser (map-parser (opt (lookahead (seq (type-token :identifier)
                                                   (end-of-input))))
                              #'identity)))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (assert-true ok)
        (assert-false value)
        (assert-equal 0 next)
        (%assert-single-diagnostic diagnostics
                                   "Unexpected trailing token"
                                   "1:4-1:5")))))
