(in-package :cl-parser-kit/test)

(register-example-test-cases
 (readme-tokenizer-quick-start-test
  (let* ((tokenizer (make-expression-example-tokenizer))
         (tokens (tokenize-string "sum + 42" tokenizer)))
    (expect (length tokens) :to-equal 3)
    (expect (token-type (aref tokens 0)) :to-equal :identifier)
    (expect (token-type (aref tokens 1)) :to-equal :plus)
    (expect (token-value (aref tokens 2)) :to-equal 42)))
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
      (expect next :to-equal 3)
      (expect failure :to-be-truthy)
      (expect (token-type (parse-failure-actual failure)) :to-equal :identifier)
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
      (expect next :to-equal 9)
      (expect (length (first value)) :to-equal 3)
      (expect (token-text (first (first value))) :to-equal "answer")
      (expect (token-text (second (first value))) :to-equal "result")
      (expect (token-text (third (first value))) :to-equal "total")))))
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
        (expect next :to-equal 4)
        (expect value :to-equal '(:add 1 (:fact 2)))))))
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
      (expect next :to-equal 2)
      (expect (length value) :to-equal 3)
      (expect (token-type (first value)) :to-equal :identifier)
      (expect (token-text (first value)) :to-equal "answer")
      (expect (token-type (second value)) :to-equal :number)
      (expect (token-value (second value)) :to-equal 42))
     ((parse-tokens
       parser
       (vector (make-token :type :identifier :text "answer" :value "answer")))
      (value next failure)
      (expect next :to-equal 1)
      (expect (length value) :to-equal 3)
      (expect (token-type (first value)) :to-equal :identifier)
      (expect (token-text (first value)) :to-equal "answer")
      (expect (second value) :to-equal nil)))))
 (api-guide-label-snippet-test
  (let ((parser
          (label
           (type-token :identifier)
           :binding-name)))
    (assert-example-failure
     (parse-tokens parser
                   (vector (make-token :type :equals :text "=")))
     (value next failure)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :binding-name)
      (expect (token-type (parse-failure-actual failure)) :to-equal :equals))))
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
       (expect next :to-equal 7)
       (expect (length (first value)) :to-equal 2)
       (expect (token-text (first (first value))) :to-equal "answer")
       (expect (token-text (second (first value))) :to-equal "result")))))
