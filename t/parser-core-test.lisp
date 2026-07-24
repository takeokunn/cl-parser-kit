(in-package :cl-parser-kit/test)

(it-sequential "parse-source-test"
  (with-parser-tokens ("answer + 1" *identifier-plus-number-rule-specs*)
      (tokenizer tokens)
    (let ((parser (seq (type-token :identifier)
                       (type-token :plus)
                       (type-token :number))))
      (assert-parser-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 3)
        (expect (token-type (first value)) :to-equal :identifier)))))

(it-sequential "parse-token-helpers-test"
  (with-parser-tokens ("answer + 1" *identifier-plus-number-rule-specs*)
      (tokenizer tokens)
    (let ((parser (type-token :identifier)))
      (assert-parser-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)))
    (let ((token (peek-token tokens 0)))
      (expect (token-type token) :to-equal :identifier))
    (multiple-value-bind (token next)
        (next-token tokens 0)
      (expect (token-type token) :to-equal :identifier)
      (expect next :to-equal 1))
    (expect (eof-token-p tokens 0) :to-be-falsy)
    (expect (eof-token-p tokens (length tokens)) :to-be-truthy)
    ;; %TOKEN-STREAM-TOKEN-AT (shared by PEEK-TOKEN/NEXT-TOKEN) returns NIL
    ;; past the end of the stream, distinct from the token-present case above.
    (expect (peek-token tokens (length tokens)) :to-be-falsy)
    (multiple-value-bind (token next)
        (next-token tokens (length tokens))
      (expect token :to-be-falsy)
      (expect next :to-equal (length tokens)))))

(it-sequential "eof-token-p-accepts-a-list-token-stream-test"
  ;; EOF-TOKEN-P's ETYPECASE has a dedicated LIST clause distinct from its
  ;; string/vector one -- length-based indexing doesn't apply to a list, so
  ;; it walks with NTH instead.
  (let ((tokens (list (make-token :type :identifier :text "a"))))
    (expect (eof-token-p tokens 0) :to-be-falsy)
    (expect (eof-token-p tokens 1) :to-be-truthy)
    ;; A negative position is not EOF either -- it is simply invalid.
    (expect (eof-token-p tokens -1) :to-be-falsy)))

(it-sequential "eof-token-p-rejects-a-tokens-argument-of-an-unsupported-type-test"
  ;; EOF-TOKEN-P's ETYPECASE covers only (OR STRING VECTOR) and LIST; a value
  ;; of neither type (so the LIST clause's own dispatch test is exercised as
  ;; false, having already fallen through the string/vector clause) must fall
  ;; through to a signalled error rather than silently returning a bogus
  ;; answer.
  (expect (lambda () (eof-token-p 42 0)) :to-throw 'error))

(it-sequential "parse-failure-test"
  (with-parser-tokens ("answer + 1" *identifier-plus-number-rule-specs*)
      (tokenizer tokens)
    (let ((parser (seq (type-token :identifier)
                       (type-token :plus)
                       (type-token :plus))))
      (assert-parser-failure (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect (parse-failure-expected failure) :to-equal :plus)
        (expect (token-type (parse-failure-actual failure)) :to-equal :number)))))

(it-sequential "parse-source-success-contract-test"
  (let* ((tokenizer (%make-parser-tokenizer *identifier-number-rule-specs*))
         (parser (type-token :identifier)))
    (assert-parser-success (parse-source parser "answer" tokenizer)
        (value next failure)
      (expect next :to-equal 1)
      (expect (token-type value) :to-equal :identifier))))

(it-sequential "parse-source-failure-contract-test"
  (let* ((tokenizer (%make-parser-tokenizer *identifier-number-rule-specs*))
         (parser (type-token :identifier)))
    (assert-parser-failure (parse-source parser "42" tokenizer)
        (value next failure)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :identifier)
      (expect (token-type (parse-failure-actual failure)) :to-equal :number))))

(it-sequential "parse-tokens-discards-success-path-diagnostics-test"
    (let* ((tokens (vector (make-token :type :identifier :text "foo" :start 0 :end 3)
                         (make-token :type :comma :text "," :start 3 :end 4)))
         (parser (seq (opt (lookahead (seq (type-token :identifier)
                                           (end-of-input))))
                      (type-token :identifier)
                      (type-token :comma))))
    (assert-parser-success (parse-tokens parser tokens)
        (value next failure)
      (expect (length value) :to-equal 3)
      (expect next :to-equal 2)
      (expect failure :to-be-falsy))))

(it-sequential "parse-all-discards-success-path-diagnostics-test"
    (let* ((tokens (vector (make-token :type :identifier :text "foo" :start 0 :end 3)
                         (make-token :type :comma :text "," :start 3 :end 4)))
         (parser (seq (opt (lookahead (seq (type-token :identifier)
                                           (end-of-input))))
                      (type-token :identifier)
                      (type-token :comma))))
    (assert-parser-success (parse-all parser tokens)
        (value next failure)
      (expect (length value) :to-equal 3)
      (expect next :to-equal 2)
      (expect failure :to-be-falsy))))

(it-sequential "parse-source-discards-success-path-diagnostics-test"
    (let* ((tokenizer (%make-parser-tokenizer *identifier-comma-rule-specs*))
         (parser (seq (opt (lookahead (seq (type-token :identifier)
                                           (end-of-input))))
                      (type-token :identifier)
                      (type-token :comma))))
    (assert-parser-success (parse-source parser "foo," tokenizer)
        (value next failure)
      (expect (length value) :to-equal 3)
      (expect next :to-equal 2)
      (expect failure :to-be-falsy))))
