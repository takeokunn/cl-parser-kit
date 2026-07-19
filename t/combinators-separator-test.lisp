(in-package :cl-parser-kit/test)

(defparameter *single-foo-token-vector* (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")
                         (make-token :type :identifier :text "bar")
                         (make-token :type :comma :text ",")
                         (make-token :type :identifier :text "baz")))

(it-sequential "combinator-sep-by1-test"
  (let* ((tokens *single-foo-token-vector*)
         (parser (sep-by1 (type-token :identifier)
                          (type-token :comma))))
    (assert-combinator-projected-values (parse-tokens parser tokens)
        (value next failure)
        5
        '("foo" "bar" "baz")
        #'token-text)))

(it-sequential "combinator-sep-by-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")
                         (make-token :type :identifier :text "bar")
                         (make-token :type :plus :text "+")))
         (parser (sep-by (type-token :identifier)
                         (type-token :comma))))
    (assert-combinator-projected-values (parse-tokens parser tokens)
        (value next failure)
        3
        '("foo" "bar")
        #'token-text))
  (let ((parser (sep-by (type-token :identifier)
                        (type-token :comma))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser #())
      (declare (ignore failure))
      (expect ok :to-be-truthy)
      (expect value :to-equal '())
      (expect next :to-equal 0))))

(it-sequential "combinator-sep-by-trailing-separator-fails-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")))
         (parser (sep-by1 (type-token :identifier)
                          (type-token :comma))))
    (assert-separator-combinator-failure (parse-tokens parser tokens)
                                         2
                                         :identifier
                                         :eof)))

(it-sequential "combinator-sep-by1-rejects-non-advancing-separator-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")))
         (separator-parser (return-parser :separator))
         (parser (sep-by1 (type-token :identifier) separator-parser)))
    (assert-separator-combinator-failure (parse-tokens parser tokens)
                                         1
                                         :progressing-parser
                                         :return)))

(it-sequential "combinator-sep-by-propagates-committed-failure-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")))
         (parser (sep-by (type-token :identifier)
                          (type-token :comma))))
    (assert-separator-combinator-failure (parse-tokens parser tokens)
                                         2
                                         :identifier
                                         :eof)))

(it-sequential "combinator-sep-end-by1-allows-trailing-separator-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")
                         (make-token :type :identifier :text "bar")
                         (make-token :type :comma :text ",")))
         (parser (sep-end-by1 (type-token :identifier)
                              (type-token :comma))))
    (assert-combinator-projected-values (parse-tokens parser tokens)
        (value next failure)
        4
        '("foo" "bar")
        #'token-text)))

(it-sequential "combinator-sep-end-by1-rejects-non-advancing-separator-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")))
         (separator-parser (return-parser :separator))
         (parser (sep-end-by1 (type-token :identifier) separator-parser)))
    (assert-separator-combinator-failure (parse-tokens parser tokens)
                                         1
                                         :progressing-parser
                                         :return)))

(it-sequential "combinator-sep-end-by1-rejects-non-advancing-item-after-separator-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")))
         (parser (sep-end-by1 (return-parser :item)
                              (type-token :comma))))
    (assert-separator-combinator-failure (run-parser parser tokens 0)
                                         0
                                         :progressing-parser
                                         :return)))

(it-sequential "combinator-sep-end-by-allows-empty-input-test"
  (let ((parser (sep-end-by (type-token :identifier)
                            (type-token :comma))))
    (assert-combinator-success (parse-tokens parser #()) (value next failure)
      (expect value :to-equal '())
      (expect next :to-equal 0))))

(it-sequential "combinator-sep-end-by1-propagates-committed-failure-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :number :text "1" :value 1)
                         (make-token :type :comma :text ",")
                         (make-token :type :identifier :text "bar")))
         (parser (sep-end-by1 (seq (type-token :identifier)
                                    (type-token :number))
                                 (type-token :comma))))
    (assert-separator-combinator-failure (parse-tokens parser tokens)
                                         4
                                         :number
                                         :eof)))

(it-sequential "combinator-sep-end-by-preserves-recoverable-diagnostics-test"
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let ((parser (sep-end-by (lookahead (seq (type-token :identifier)
                                              (end-of-input)))
                              (type-token :comma))))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (expect ok :to-be-truthy)
        (expect value :to-equal '())
        (expect next :to-equal 0)
        (%assert-rendered-diagnostic-contains (first diagnostics)
                                              "Unexpected trailing token"
                                              "1:4-1:5")))))

(it-sequential "combinator-sep-by-allows-non-consuming-lookahead-failure-test"
  (with-combinator-tokens (tokens *identifier-comma-token-specs*)
    (let ((parser (sep-by (lookahead (seq (type-token :identifier)
                                          (type-token :plus)))
                          (type-token :comma))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect value :to-equal '())
        (expect next :to-equal 0)))))

(it-sequential "combinator-sep-by-preserves-recoverable-diagnostics-test"
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let ((parser (sep-by (lookahead (seq (type-token :identifier)
                                          (end-of-input)))
                          (type-token :comma))))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (expect ok :to-be-truthy)
        (expect value :to-equal '())
        (expect next :to-equal 0)
        (%assert-single-diagnostic diagnostics
                                   "Unexpected trailing token"
                                   "1:4-1:5")))))
