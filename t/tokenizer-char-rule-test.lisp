(in-package :cl-parser-kit/test)

(it-sequential "char-rule-matches-specific-character-test"
  (let* ((tokenizer (make-tokenizer :rules (list (make-char-rule :lparen #\())))
         (tokens (tokenize-string "(" tokenizer)))
    (expect (length tokens) :to-equal 1)
    (expect (token-type (elt tokens 0)) :to-equal :lparen)
    (expect (token-text (elt tokens 0)) :to-equal "(")))

(it-sequential "char-rule-matches-any-character-in-a-set-test"
  (let* ((tokenizer (make-tokenizer :rules (list (make-char-rule :op "+-*"))))
         (tokens (tokenize-string "+*-" tokenizer)))
    (expect (map 'list #'token-type tokens) :to-equal '(:op :op :op))
    (expect (map 'list #'token-text tokens) :to-equal '("+" "*" "-"))))

(it-sequential "char-rule-uses-predicate-and-value-function-test"
  (let* ((tokenizer (make-tokenizer
                     :rules (list (make-char-rule :digit #'digit-char-p
                                                  :value-function (lambda (text)
                                                                    (parse-integer text))))))
         (tokens (tokenize-string "7" tokenizer)))
    (expect (token-type (elt tokens 0)) :to-equal :digit)
    (expect (token-value (elt tokens 0)) :to-equal 7)))

(it-sequential "char-rule-declines-a-character-outside-the-set-test"
  ;; %COERCE-CHAR-PREDICATE's SEQUENCE clause must decline (not just match) a
  ;; character that is not a member of the set, falling through to the next
  ;; rule instead of misfiring.
  (let* ((tokenizer (make-tokenizer :rules (list (make-char-rule :op "+-*")
                                                 (make-identifier-rule))))
         (tokens (tokenize-string "a" tokenizer)))
    (expect (token-type (elt tokens 0)) :to-equal :identifier)))

(it-sequential "char-rule-matcher-declines-at-out-of-bounds-index-test"
  ;; TOKEN-RULE-MATCHER is public API; MAKE-CHAR-RULE's own EOF guard must
  ;; decline gracefully rather than index out of bounds when invoked directly
  ;; past the end of SOURCE.
  (let ((rule (make-char-rule :bang #\!)))
    (expect (funcall (token-rule-matcher rule) "!" 1) :to-be-falsy)))

(it-sequential "char-rule-rejects-a-spec-of-an-unsupported-type-test"
  ;; %COERCE-CHAR-PREDICATE's ETYPECASE covers only CHARACTER, FUNCTION, and
  ;; SEQUENCE; a value of none of those (so the SEQUENCE clause's own dispatch
  ;; test is exercised as false, having already fallen through the character
  ;; and function clauses) must fall through to a signalled error.
  (expect (lambda () (make-char-rule :bad 42)) :to-throw 'error))

(it-sequential "char-rule-skip-p-discards-match-test"
  (let* ((tokenizer (make-tokenizer
                     :rules (list (make-char-rule :space #\Space :skip-p t)
                                  (make-char-rule :bang #\!))))
         (tokens (tokenize-string " ! " tokenizer)))
    (expect (length tokens) :to-equal 1)
    (expect (token-type (elt tokens 0)) :to-equal :bang)))
