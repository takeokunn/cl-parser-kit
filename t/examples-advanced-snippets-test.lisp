(in-package :cl-parser-kit/test)

(register-example-test-cases
 (examples-guide-parse-tokens-snippet-test
  (assert-example-success
   (parse-tokens
    (seq
     (label
      (type-token :identifier)
      :binding-name)
     (literal "=" :type :equals)
     (type-token :number)
     (end-of-input))
    (vector (make-token :type :identifier :text "answer")
            (make-token :type :equals :text "=")
            (make-token :type :number :text "42" :value 42)))
   (value next failure)
   (assert-equal 3 next)
   (assert-equal "answer" (token-text (first value)))
   (assert-equal 42 (token-value (third value)))))
 (examples-guide-tokenize-dsl-flavored-source-snippet-test
  (assert-dsl-sample-tokens))
 (examples-guide-parse-tokens-snippet-failure-test
  (assert-example-values
   (parse-tokens
    (seq
     (label
      (type-token :identifier)
      :binding-name)
     (literal "=" :type :equals)
     (type-token :number)
     (end-of-input))
    (vector (make-token :type :number :text "42" :value 42)))
   (ok value next failure)
   (declare (ignore ok value next))
   (assert-false ok)
   (assert-equal 0 (parse-failure-position failure))
   (assert-equal :binding-name (parse-failure-expected failure))
   (assert-equal :number
                 (token-type (parse-failure-actual failure)))))
 (examples-guide-shape-failure-snippet-test
  (assert-example-shape-failure-snippet))
 (examples-guide-operator-chain-snippet-test
  (let* ((tokenizer (make-operator-chain-tokenizer))
         (number-parser
           (map-parser
            (type-token :number)
            #'token-value))
         (subtract-parser
           (chainl1
            number-parser
            (operator-parser
             (literal "-" :type :minus)
             (lambda (left right)
               (- left right)))))
         (power-parser
           (chainr1
            number-parser
            (operator-parser
             (literal "^" :type :caret)
             (lambda (left right)
               (expt left right))))))
    (assert-example-successes
     ((parse-source subtract-parser "10 - 3 - 2" tokenizer)
      (value next failure)
      (assert-equal 5 next)
      (assert-equal 5 value))
     ((parse-source power-parser "2 ^ 3 ^ 2" tokenizer)
      (value next failure)
      (assert-equal 5 next)
      (assert-equal 512 value)))))
 (examples-guide-project-token-text-and-values-snippet-test
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
 (examples-guide-render-failure-from-external-tokens-snippet-test
  (let* ((source "answer
+")
         (tokens (vector (make-token :type :identifier
                                     :text "answer"
                                     :start 0
                                     :end 6
                                     :metadata (list :source source))
                         (make-token :type :plus
                                     :text "+"
                                     :start 7
                                     :end 8
                                     :metadata (list :source source))))
         (parser (type-token :identifier))
         (rendered
           (multiple-value-bind (ok value next failure)
               (parse-all parser tokens)
             (declare (ignore value next))
             (if ok
                 :ok
                 (parse-failure->string failure)))))
    (assert-true (stringp rendered))
    (assert-string-contains-all
     rendered
     '("Unexpected trailing token"
       "2:1-2:2"
       "  | +"))))
 (examples-guide-parse-source-end-to-end-snippet-test
  (let* ((tokenizer (make-let-example-tokenizer))
         (parser (seq
                  (literal "let" :type :let)
                  (delimited-sep-by1
                   (literal "(" :type :lparen)
                   (type-token :identifier)
                   (literal "," :type :comma)
                   (literal ")" :type :rparen))
                  (opt (literal ";" :type :semicolon))
                  (end-of-input))))
    (assert-example-success
     (parse-source parser "let (answer, result, total);" tokenizer)
     (value next failure)
     (assert-equal 9 next)
     (assert-equal "answer" (token-text (first (second value))))
     (assert-equal "result" (token-text (second (second value))))
     (assert-equal "total" (token-text (third (second value)))))))
 (examples-guide-render-parse-failure-snippet-test
  (with-pratt-diagnostic-context (tokenizer table)
    (assert-pratt-failure-rendering ("1 + +" tokenizer table)
      '("Expected PREFIX" "1 + +" "^"))))
 (examples-guide-build-manual-diagnostic-snippet-test
  (assert-diagnostic-sample-rendering))
 (examples-guide-build-and-inspect-cst-snippet-test
  (let* ((tokenizer (make-let-example-tokenizer))
         (parser (seq
                  (literal "let" :type :let)
                  (type-token :identifier)
                  (literal "=" :type :equals)
                  (type-token :number)
                  (opt (literal ";" :type :semicolon))
                  (end-of-input))))
    (let ((result
            (multiple-value-bind (ok value)
                (parse-source parser "let answer = 42;" tokenizer)
              (declare (ignore value))
              (when ok
                (let ((cst (make-cst-node
                            :type :binding
                            :children (list (make-cst-node :type :keyword :value "let")
                                            (make-cst-node :type :identifier :value "answer")
                                            (make-cst-node :type :punctuation :value "=")
                                            (make-cst-node :type :number :value "42")
                                            (make-cst-node :type :punctuation :value ";")))))
                  (cst-node->sexp cst))))))
      (assert-equal '(:type :binding
                      :value nil
                      :children ((:type :keyword :value "let" :children ())
                                 (:type :identifier :value "answer" :children ())
                                 (:type :punctuation :value "=" :children ())
                                 (:type :number :value "42" :children ())
                                 (:type :punctuation :value ";" :children ())))
                    result)))))
