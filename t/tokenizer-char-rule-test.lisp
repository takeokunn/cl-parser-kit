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

(it-sequential "char-rule-skip-p-discards-match-test"
  (let* ((tokenizer (make-tokenizer
                     :rules (list (make-char-rule :space #\Space :skip-p t)
                                  (make-char-rule :bang #\!))))
         (tokens (tokenize-string " ! " tokenizer)))
    (expect (length tokens) :to-equal 1)
    (expect (token-type (elt tokens 0)) :to-equal :bang)))
