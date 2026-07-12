(in-package :cl-parser-kit/test)

(deftest-case combinator-opt-test
  (with-combinator-tokens (tokens *identifier-only-token-specs*)
    (let ((parser (opt (type-token :identifier))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (assert-equal 1 next)
        (assert-equal :identifier (token-type value)))))
  (let ((parser (opt (type-token :identifier))))
    (assert-combinator-success (parse-tokens parser #())
        (value next failure)
      (assert-false value)
      (assert-equal 0 next))))

(deftest-case combinator-opt-propagates-progressed-failure-test
  (let* ((tokens (vector (make-token :type :plus :text "+")))
         (parser (opt (seq (type-token :plus)
                           (type-token :number)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (assert-false ok)
      (assert-false value)
      (assert-equal 1 next)
      (assert-equal 1 (parse-failure-position failure))
      (assert-equal :number (parse-failure-expected failure))
      (assert-equal :eof (parse-failure-actual failure)))))

(deftest-case combinator-opt-keeps-lookahead-failure-recoverable-test
  (with-combinator-tokens (tokens *identifier-comma-token-specs*)
    (let ((parser (opt (lookahead (seq (type-token :identifier)
                                       (type-token :plus))))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (assert-false value)
        (assert-equal 0 next)))))

(deftest-case combinator-opt-preserves-recoverable-diagnostics-test
  (with-combinator-tokens (tokens *positioned-identifier-comma-token-specs*)
    (let ((parser (opt (lookahead (seq (type-token :identifier)
                                       (end-of-input))))))
      (multiple-value-bind (ok value next diagnostics)
          (run-parser parser tokens 0)
        (assert-true ok)
        (assert-false value)
        (assert-equal 0 next)
        (%assert-single-diagnostic diagnostics
                                   "Unexpected trailing token"
                                   "1:4-1:5")))))

(deftest-case combinator-label-overrides-expected-test
  (let* ((tokens (vector (make-token :type :number :text "42" :value 42)))
         (parser (label (type-token :identifier) :binding-name)))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (assert-false ok)
      (assert-false value)
      (assert-equal 0 next)
      (assert-equal :binding-name (parse-failure-expected failure))
      (assert-equal :number (token-type (parse-failure-actual failure))))))

(deftest-case combinator-label-alias-test
  (let ((parser (label (type-token :identifier) "binding name")))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser #())
      (assert-false ok)
      (assert-false value)
      (assert-equal 0 next)
      (assert-equal "binding name" (parse-failure-expected failure))
      (assert-equal :eof (parse-failure-actual failure)))))

(deftest-case combinator-alt-propagates-farthest-failure-test
  (let* ((tokens (vector (make-token :type :identifier :text "lhs")
                         (make-token :type :identifier :text "rhs")))
         (parser (alt (seq (type-token :identifier) (type-token :equals))
                      (seq (type-token :identifier)
                           (type-token :identifier)
                           (type-token :rparen)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (assert-false ok)
      (assert-false value)
      (assert-equal 2 next)
      (assert-equal 2 (parse-failure-position failure))
      (assert-equal :rparen
                    (parse-failure-expected failure)))))

(deftest-case combinator-alt-merges-expectations-at-same-position-test
  (let* ((tokens (vector (make-token :type :identifier :text "lhs")
                         (make-token :type :comma :text ",")))
         (parser (alt (seq (type-token :identifier) (type-token :equals))
                      (seq (type-token :identifier) (type-token :rparen)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (assert-false ok)
      (assert-false value)
      (assert-equal 1 next)
      (assert-equal 1 (parse-failure-position failure))
      (assert-equal '(:equals :rparen)
                    (parse-failure-expected failure)))))

(deftest-case combinator-lookahead-test
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :plus :text "+")))
         (parser (lookahead (seq (type-token :identifier)
                                 (type-token :plus)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (declare (ignore failure))
      (assert-true ok)
      (assert-equal 0 next)
      (assert-equal 2 (length value)))))

(deftest-case combinator-lookahead-preserves-farthest-failure-position-test
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :comma :text ",")))
         (parser (lookahead (seq (type-token :identifier)
                                 (type-token :plus)))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser tokens)
      (assert-false ok)
      (assert-false value)
      (assert-equal 1 next)
      (assert-equal 1 (parse-failure-position failure))
      (assert-equal :plus (parse-failure-expected failure))
      (assert-equal :comma (token-type (parse-failure-actual failure))))))

(deftest-case combinator-not-followed-by-test
  (with-combinator-tokens (tokens *identifier-only-token-specs*)
    (let ((parser (not-followed-by (type-token :plus))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (assert-true value)
        (assert-equal 0 next))))
  (with-combinator-tokens (tokens *positioned-identifier-token-with-span-specs*)
    (let ((parser (not-followed-by (type-token :identifier))))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (assert-equal 0 next)
        (assert-equal :not-followed-by (parse-failure-expected failure))
        (assert-equal :identifier (token-type (parse-failure-actual failure)))
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected token"
                                   "foo")))))

(deftest-case combinator-not-followed-by-falls-back-to-token-offsets-test
  (with-combinator-tokens (tokens *offset-identifier-token-specs*)
    (let ((parser (not-followed-by (type-token :identifier))))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (assert-equal 0 next)
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected token"
                                   "1:5-1:8")))))

(deftest-case combinator-not-followed-by-recovers-line-columns-from-metadata-source-test
  (let* ((source "ab
foo")
         (parser (not-followed-by (type-token :identifier))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens parser (vector (make-token :type :identifier
                                                 :text "foo"
                                                 :start 3
                                                 :end 6
                                                 :metadata (list :source source))))
      (assert-false ok)
      (assert-false value)
      (assert-equal 0 next)
      (let* ((diagnostic (first (parse-failure-diagnostics failure)))
             (span (diagnostic-span diagnostic))
             (rendered (diagnostic->string diagnostic)))
        (assert-true diagnostic)
        (assert-equal 2 (span-start-line span))
        (assert-equal 1 (span-start-column span))
        (assert-equal 2 (span-end-line span))
        (assert-equal 4 (span-end-column span))
        (assert-true (search "Unexpected token" rendered))
        (assert-true (search "2:1-2:4" rendered))
        (assert-true (search "  | foo" rendered))))))

(deftest-case combinator-end-of-input-test
  (let ((parser (seq (type-token :identifier)
                     (end-of-input))))
    (with-combinator-tokens (tokens *identifier-only-token-specs*)
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (assert-equal 1 next)
        (assert-equal :identifier (token-type (first value))))))
  (with-combinator-tokens (tokens *positioned-identifier-token-with-span-specs*)
    (let ((parser (end-of-input)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (assert-equal 0 next)
        (assert-equal :eoi (parse-failure-expected failure))
        (assert-equal :identifier (token-type (parse-failure-actual failure)))
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected trailing token"
                                   "foo")))))

(deftest-case combinator-end-of-input-falls-back-to-token-offsets-test
  (with-combinator-tokens (tokens *offset-identifier-token-near-eoi-specs*)
    (let ((parser (end-of-input)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (assert-equal 0 next)
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "Unexpected trailing token"
                                   "1:3-1:6")))))
