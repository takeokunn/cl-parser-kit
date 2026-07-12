(in-package :cl-parser-kit/test)

(register-example-test-cases
 (readme-seq-helper-projection-snippet-test
  (with-punctuated-example-parsers (tokenizer group-parser binding-parser)
    (assert-example-successes
     ((parse-source group-parser "(answer, result);" tokenizer)
      (value next failure)
      (assert-equal 6 next)
      (assert-equal '("answer" "result") value))
     ((parse-tokens
       binding-parser
       (vector (make-token :type :identifier :text "answer")
               (make-token :type :equals :text "=" :value :assign)
               (make-token :type :number :text "42" :value 42)
               (make-token :type :semicolon :text ";")))
      (value next failure)
      (assert-equal 4 next)
      (assert-equal '("answer" :assign 42) value)))))
 (examples-guide-opt-trailing-separator-snippet-test
  (let* ((tokenizer (make-punctuated-example-tokenizer))
         (parser
           (delimited-sep-end-by
            (literal "(" :type :lparen)
            (type-token-text :identifier)
            (literal "," :type :comma)
            (literal ")" :type :rparen))))
    (assert-example-successes
     ((parse-source parser "(answer, result)" tokenizer)
      (value next failure)
      (assert-equal 5 next)
      (assert-equal '("answer" "result") value))
     ((parse-source parser "(answer, result,)" tokenizer)
      (value next failure)
      (assert-equal 6 next)
      (assert-equal '("answer" "result") value)))))
 (readme-parse-tokens-snippet-test
  (let* ((tokens (vector (make-token :type :identifier :text "answer")
                         (make-token :type :equals :text "=")
                         (make-token :type :number :text "42" :value 42)))
         (parser (seq
                  (type-token :identifier)
                  (type-token :equals)
                  (type-token :number))))
    (assert-example-success
     (parse-tokens parser tokens)
     (value next failure)
     (assert-equal 3 next)
     (assert-equal "answer" (token-text (first value)))
     (assert-equal "=" (token-text (second value)))
     (assert-equal 42 (token-value (third value))))))
 (readme-token-navigation-snippet-test
  (let* ((tokens (vector (make-token :type :identifier :text "answer")
                         (make-token :type :equals :text "=")
                         (make-token :type :number :text "42" :value 42)))
         (parser
           (map-parser
            (seq
             (satisfies-token
              (lambda (token)
                (and (eql (token-type token) :identifier)
                     (> (length (token-text token)) 3)))
              :expected-name :long-identifier)
             (type-token :equals)
             (type-token-value :number)
             (end-of-input))
            (lambda (parts)
              (list (token-text (first parts))
                    (third parts))))))
    (multiple-value-bind (first next)
        (next-token tokens 0)
      (assert-equal "answer" (token-text (peek-token tokens 0)))
      (assert-equal "answer" (token-text first))
      (assert-equal 1 next)
      (assert-false (eof-token-p tokens next))
      (assert-example-success
       (parse-tokens parser tokens)
       (value parse-next failure)
       (assert-equal 3 parse-next)
       (assert-equal '("answer" 42) value)
       (assert-false failure))
      (assert-true (eof-token-p tokens (length tokens)))))))
