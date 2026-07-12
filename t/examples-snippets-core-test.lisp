(in-package :cl-parser-kit/test)

(register-example-test-cases
 (readme-tokenizer-quick-start-test
  (let* ((tokenizer (make-expression-example-tokenizer))
         (tokens (tokenize-string "sum + 42" tokenizer)))
    (assert-equal 3 (length tokens))
    (assert-equal :identifier (token-type (aref tokens 0)))
    (assert-equal :plus (token-type (aref tokens 1)))
    (assert-equal 42 (token-value (aref tokens 2)))))
 (readme-tokenizer-customization-snippet-test
  (assert-dsl-sample-tokens))
 (token-stream-example-workflow-test
  (let* ((tokenizer (make-punctuated-example-tokenizer))
         (tokens (tokenize-string "answer = 42 extra" tokenizer))
         (parser (seq
                  (type-token :identifier)
                  (literal "=" :type :equals)
                  (type-token :number))))
    (assert-example-failure
     (parse-all parser tokens)
     (value next failure)
      (assert-equal 3 next)
      (assert-true failure)
      (assert-equal :identifier (token-type (parse-failure-actual failure)))
      (assert-string-contains-all
       (parse-failure->string failure)
       '("Unexpected trailing token"
         "answer = 42 extra"
         "^^^^^")))))
 (examples-end-to-end-parser-workflow-test
  (let* ((tokenizer (make-let-example-tokenizer))
         (identifier-list-parser
           (delimited-sep-by1
            (literal "(" :type :lparen)
            (type-token :identifier)
            (literal "," :type :comma)
            (literal ")" :type :rparen)))
         (parser (seq
                  (preceded-by
                   (literal "let" :type :let)
                   identifier-list-parser)
                  (opt (literal ";" :type :semicolon))
                  (end-of-input))))
    (assert-example-successes
     ((parse-source parser "let (answer, result, total);" tokenizer)
      (value next failure)
      (assert-equal 9 next)
      (assert-equal 3 (length (first value)))
      (assert-equal "answer" (token-text (first (first value))))
      (assert-equal "result" (token-text (second (first value))))
      (assert-equal "total" (token-text (third (first value))))))))
 (readme-pratt-quick-start-test
  (let ((tokens (vector (make-token :type :number :text "1" :value 1)
                        (make-token :type :plus :text "+")
                        (make-token :type :number :text "2" :value 2)
                        (make-token :type :bang :text "!"))))
    (with-pratt-plus-table (table)
      (register-postfix-operator table :bang 30 #'%pratt-fact-led)
      (assert-example-success
       (parse-pratt-all tokens table)
       (value next failure)
        (assert-equal 4 next)
        (assert-equal '(:add 1 (:fact 2)) value)))))
 (api-guide-parser-primitives-snippet-test
  (let ((parser (seq
                 (type-token :identifier)
                 (opt (type-token :number))
                 (end-of-input))))
    (assert-example-successes
     ((parse-tokens
       parser
       (vector (make-token :type :identifier :text "answer" :value "answer")
               (make-token :type :number :text "42" :value 42)))
      (value next failure)
      (assert-equal 2 next)
      (assert-equal 3 (length value))
      (assert-equal :identifier (token-type (first value)))
      (assert-equal "answer" (token-text (first value)))
      (assert-equal :number (token-type (second value)))
      (assert-equal 42 (token-value (second value))))
     ((parse-tokens
       parser
       (vector (make-token :type :identifier :text "answer" :value "answer")))
      (value next failure)
      (assert-equal 1 next)
      (assert-equal 3 (length value))
      (assert-equal :identifier (token-type (first value)))
      (assert-equal "answer" (token-text (first value)))
      (assert-equal nil (second value))))))
 (api-guide-label-snippet-test
  (let ((parser
          (label
           (type-token :identifier)
           :binding-name)))
    (assert-example-failure
     (parse-tokens parser
                   (vector (make-token :type :equals :text "=")))
     (value next failure)
      (assert-equal 0 next)
      (assert-equal :binding-name (parse-failure-expected failure))
      (assert-equal :equals (token-type (parse-failure-actual failure))))))
  (readme-delimited-list-snippet-test
   (let* ((tokenizer (make-let-example-tokenizer))
          (parser (seq
                   (preceded-by
                    (literal "let" :type :let)
                    (delimited-sep-by1
                     (literal "(" :type :lparen)
                     (type-token :identifier)
                     (literal "," :type :comma)
                     (literal ")" :type :rparen)))
                   (opt (literal ";" :type :semicolon))
                   (end-of-input))))
     (assert-example-success
      (parse-source parser "let (answer, result);" tokenizer)
      (value next failure)
       (assert-equal 7 next)
       (assert-equal 2 (length (first value)))
       (assert-equal "answer" (token-text (first (first value))))
       (assert-equal "result" (token-text (second (first value))))))))
