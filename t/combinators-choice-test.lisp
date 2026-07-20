(in-package :cl-parser-kit/test)

;;; CHOICE --------------------------------------------------------------------

(it-sequential "combinator-choice-returns-first-success-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (choice (list (type-token :plus)
                                (type-token :identifier)))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type value) :to-equal :identifier)))))

(it-sequential "combinator-choice-empty-list-fails-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (choice '())))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal :alternative)))))

;;; OPTION --------------------------------------------------------------------

(it-sequential "combinator-option-returns-default-when-absent-test"
  (with-combinator-tokens (tokens '((:type :comma :text ",")))
    (let ((parser (option :missing (type-token :identifier))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 0)
        (expect value :to-equal :missing)))))

(it-sequential "combinator-option-returns-value-when-present-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (option :missing (type-token :identifier))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type value) :to-equal :identifier)))))

(it-sequential "combinator-option-propagates-committed-failure-test"
  ;; The defining property versus a naive (ALT P (RETURN-PARSER DEFAULT)):
  ;; a committed inner failure must NOT be replaced by the default.
  (let* ((tokens (vector (make-token :type :plus :text "+")))
         (parser (option :missing (seq (type-token :plus) (type-token :number)))))
    (assert-combinator-failure (parse-tokens parser tokens)
        (value next failure)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-expected failure) :to-equal :number))))

;;; FAIL-PARSER ---------------------------------------------------------------

(it-sequential "combinator-fail-parser-always-fails-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (fail-parser "not allowed here")))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 0)
        (expect (parse-failure-expected failure) :to-equal :failure)
        (expect (parse-failure-committed-p failure) :to-be-falsy)
        (%assert-single-diagnostic (parse-failure-diagnostics failure)
                                   "not allowed here")))))

(it-sequential "combinator-fail-parser-honors-expected-override-test"
  (let ((parser (fail-parser "reserved word" :expected :reserved)))
    (assert-combinator-failure (parse-tokens parser #())
        (value next failure)
      (expect (parse-failure-expected failure) :to-equal :reserved))))

;;; AS-VALUE ------------------------------------------------------------------

(it-sequential "combinator-as-value-replaces-result-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (as-value :keyword-marker (type-token :identifier))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect value :to-equal :keyword-marker)))))

(it-sequential "combinator-as-value-fails-with-parser-test"
  (let ((parser (as-value :marker (type-token :identifier))))
    (assert-combinator-failure (parse-tokens parser #())
        (value next failure)
      (expect (parse-failure-expected failure) :to-equal :identifier))))

;;; PURE ----------------------------------------------------------------------

(it-sequential "combinator-pure-consumes-nothing-test"
  (let ((parser (pure 42)))
    (assert-combinator-success (parse-tokens parser #())
        (value next failure)
      (expect next :to-equal 0)
      (expect value :to-equal 42))))
