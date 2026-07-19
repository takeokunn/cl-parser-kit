(in-package :cl-parser-kit/test)

(it-sequential "combinator-opt-test"
  (with-combinator-tokens (tokens *identifier-only-token-specs*)
    (let ((parser (opt (type-token :identifier))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type value) :to-equal :identifier))))
  (let ((parser (opt (type-token :identifier))))
    (assert-combinator-success (parse-tokens parser #())
        (value next failure)
      (expect value :to-be-falsy)
      (expect next :to-equal 0))))

(it-sequential "combinator-opt-propagates-progressed-failure-test"
  (let* ((tokens (vector (make-token :type :plus :text "+")))
         (parser (opt (seq (type-token :plus)
                           (type-token :number)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 1)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-expected failure) :to-equal :number)
      (expect (parse-failure-actual failure) :to-equal :eof))))

(it-sequential "combinator-opt-keeps-lookahead-failure-recoverable-test"
  (with-combinator-tokens (tokens *identifier-comma-token-specs*)
    (let ((parser (opt (lookahead (seq (type-token :identifier)
                                       (type-token :plus))))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect value :to-be-falsy)
        (expect next :to-equal 0)))))

(it-sequential "combinator-opt-preserves-recoverable-diagnostics-test"
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let ((parser (opt (lookahead (seq (type-token :identifier)
                                       (end-of-input))))))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (expect ok :to-be-truthy)
        (expect value :to-be-falsy)
        (expect next :to-equal 0)
        (%assert-single-diagnostic diagnostics
                                   "Unexpected trailing token"
                                   "1:4-1:5")))))

(it-sequential "combinator-label-overrides-expected-test"
  (let* ((tokens (vector (make-token :type :number :text "42" :value 42)))
         (parser (label (type-token :identifier) :binding-name)))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :binding-name)
      (expect (token-type (parse-failure-actual failure)) :to-equal :number))))

(it-sequential "combinator-label-alias-test"
  (let ((parser (label (type-token :identifier) "binding name")))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser #())
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal "binding name")
      (expect (parse-failure-actual failure) :to-equal :eof))))

(it-sequential "combinator-alt-propagates-farthest-failure-test"
  (let* ((tokens (vector (make-token :type :identifier :text "lhs")
                         (make-token :type :identifier :text "rhs")))
         (parser (alt (seq (type-token :identifier) (type-token :equals))
                      (seq (type-token :identifier)
                           (type-token :identifier)
                           (type-token :rparen)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 2)
      (expect (parse-failure-position failure) :to-equal 2)
      (expect (parse-failure-expected failure) :to-equal :rparen))))

(it-sequential "combinator-alt-merges-expectations-at-same-position-test"
  (let* ((tokens (vector (make-token :type :identifier :text "lhs")
                         (make-token :type :comma :text ",")))
         (parser (alt (seq (type-token :identifier) (type-token :equals))
                      (seq (type-token :identifier) (type-token :rparen)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 1)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-expected failure) :to-equal '(:equals :rparen)))))

(it-sequential "combinator-lookahead-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :plus :text "+")))
         (parser (lookahead (seq (type-token :identifier)
                                 (type-token :plus)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (declare (ignore failure))
      (expect ok :to-be-truthy)
      (expect next :to-equal 0)
      (expect (length value) :to-equal 2))))

(it-sequential "combinator-lookahead-preserves-farthest-failure-position-test"
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")))
         (parser (lookahead (seq (type-token :identifier)
                                 (type-token :plus)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 1)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-expected failure) :to-equal :plus)
      (expect (token-type (parse-failure-actual failure)) :to-equal :comma))))

(it-sequential "combinator-not-followed-by-test"
  (with-combinator-tokens (tokens *identifier-only-token-specs*)
    (let ((parser (not-followed-by (type-token :plus))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect value :to-be-truthy)
        (expect next :to-equal 0))))
  (with-combinator-tokens (tokens *positioned-identifier-token-with-span-specs*)
    (let ((parser (not-followed-by (type-token :identifier))))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 0)
        (expect (parse-failure-expected failure) :to-equal :not-followed-by)
        (expect (token-type (parse-failure-actual failure)) :to-equal :identifier)
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected token"
                                   "foo")))))

(it-sequential "combinator-not-followed-by-falls-back-to-token-offsets-test"
  (with-combinator-tokens (tokens *offset-identifier-token-specs*)
    (let ((parser (not-followed-by (type-token :identifier))))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 0)
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected token"
                                   "1:5-1:8")))))

(it-sequential "combinator-not-followed-by-recovers-line-columns-from-metadata-source-test"
  (let* ((source "ab
foo")
         (parser (not-followed-by (type-token :identifier))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser (vector (make-token :type :identifier
                                                 :text "foo"
                                                 :start 3
                                                 :end 6
                                                 :metadata (list :source source))))
      (expect ok :to-be-falsy)
      (expect value :to-be-falsy)
      (expect next :to-equal 0)
      (let* ((diagnostic (first (parse-failure-diagnostics failure)))
             (span (diagnostic-span diagnostic))
             (rendered (diagnostic->string diagnostic)))
        (expect diagnostic :to-be-truthy)
        (expect (span-start-line span) :to-equal 2)
        (expect (span-start-column span) :to-equal 1)
        (expect (span-end-line span) :to-equal 2)
        (expect (span-end-column span) :to-equal 4)
        (expect (search "Unexpected token" rendered) :to-be-truthy)
        (expect (search "2:1-2:4" rendered) :to-be-truthy)
        (expect (search "  | foo" rendered) :to-be-truthy)))))

(it-sequential "combinator-end-of-input-test"
  (let ((parser (seq (type-token :identifier)
                     (end-of-input))))
    (with-combinator-tokens (tokens *identifier-only-token-specs*)
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type (first value)) :to-equal :identifier))))
  (with-combinator-tokens (tokens *positioned-identifier-token-with-span-specs*)
    (let ((parser (end-of-input)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 0)
        (expect (parse-failure-expected failure) :to-equal :eoi)
        (expect (token-type (parse-failure-actual failure)) :to-equal :identifier)
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected trailing token"
                                   "foo")))))

(it-sequential "combinator-end-of-input-falls-back-to-token-offsets-test"
  (with-combinator-tokens (tokens *offset-identifier-token-near-eoi-specs*)
    (let ((parser (end-of-input)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 0)
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected trailing token"
                                   "1:3-1:6")))))
