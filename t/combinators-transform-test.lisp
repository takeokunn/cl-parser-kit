(in-package :cl-parser-kit/test)

(it-sequential "combinator-bind-parser-preserves-failure-position-test"
  (let* ((parser (bind-parser (type-token :identifier)
                              (lambda (_token)
                                (declare (ignore _token))
                                (type-token :number))))
         (tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :plus :text "+"))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 1)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-expected failure) :to-equal :number))))

(it-sequential "combinator-bind-parser-preserves-success-diagnostics-test"
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let* ((diagnostic-parser (opt (lookahead (seq (type-token :identifier)
                                                   (end-of-input)))))
           (parser (bind-parser diagnostic-parser
                                (lambda (_value)
                                  (declare (ignore _value))
                                  diagnostic-parser))))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (expect ok :to-be-truthy)
        (expect value :to-be-falsy)
        (expect next :to-equal 0)
        (expect (length diagnostics) :to-equal 2)
        (dolist (diagnostic diagnostics)
          (%assert-rendered-diagnostic-contains diagnostic
                                                "Unexpected trailing token"
                                                "1:4-1:5"))))))

(it-sequential "combinator-map-parser-preserves-failure-position-test"
  (let* ((parser (map-parser (seq (type-token :identifier)
                                  (type-token :number))
                             #'identity))
         (tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :plus :text "+"))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 1)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-expected failure) :to-equal :number))))

(it-sequential "combinator-map-parser-preserves-success-diagnostics-test"
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let ((parser (map-parser (opt (lookahead (seq (type-token :identifier)
                                                   (end-of-input))))
                              #'identity)))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (expect ok :to-be-truthy)
        (expect value :to-be-falsy)
        (expect next :to-equal 0)
        (%assert-single-diagnostic diagnostics
                                   "Unexpected trailing token"
                                   "1:4-1:5")))))
