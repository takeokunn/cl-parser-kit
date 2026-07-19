(in-package :cl-parser-kit/test)

(register-example-test-cases
 (pratt-diagnostic-example-test
  (with-pratt-diagnostic-context (tokenizer table)
    (multiple-value-bind (ok value next failure)
        (parse-pratt-source "1 + +" tokenizer table)
      (declare (ignore value))
      (expect ok :to-be-falsy)
      (expect next :to-equal 2)
      (expect failure :to-be-truthy)
      (assert-string-contains-all
       (parse-failure->string failure)
      '("Expected PREFIX"
        "1 + +"
        "^")))))
 (mini-language-parser-example-test
  (let* ((tokenizer (make-let-example-tokenizer))
         (parser (seq
                  (literal "let" :type :let)
                  (type-token :identifier)
                  (literal "=" :type :equals)
                  (type-token :number)
                  (opt (literal ";" :type :semicolon))
                  (end-of-input))))
    (multiple-value-bind (ok value next failure)
        (parse-source parser "let answer = 42;" tokenizer)
      (declare (ignore failure))
      (expect ok :to-be-truthy)
      (expect next :to-equal 5)
      (expect (token-type (first value)) :to-equal :let)
      (expect (token-text (second value)) :to-equal "answer")
      (expect (token-value (fourth value)) :to-equal 42))))
 (cst-example-workflow-test
  (let* ((tokenizer (make-let-example-tokenizer))
         (parser (seq
                  (literal "let" :type :let)
                  (type-token :identifier)
                  (literal "=" :type :equals)
                  (type-token :number)
                  (opt (literal ";" :type :semicolon))
                  (end-of-input))))
    (multiple-value-bind (ok value next failure)
        (parse-source parser "let answer = 42;" tokenizer)
      (declare (ignore failure))
      (expect ok :to-be-truthy)
      (expect next :to-equal 5)
      (let* ((let-token (first value))
             (identifier-token (second value))
             (equals-token (third value))
             (number-token (fourth value))
             (semicolon-token (fifth value))
             (cst (make-cst-node
                   :type :binding
                   :span (span-merge (token-span let-token) (token-span semicolon-token))
                   :children (list (make-cst-node :type :keyword
                                                  :value (token-text let-token)
                                                  :span (token-span let-token))
                                   (make-cst-node :type :identifier
                                                  :value (token-text identifier-token)
                                                  :span (token-span identifier-token))
                                   (make-cst-node :type :punctuation
                                                  :value (token-text equals-token)
                                                  :span (token-span equals-token))
                                   (make-cst-node :type :number
                                                  :value (token-text number-token)
                                                  :span (token-span number-token))
                                   (make-cst-node :type :punctuation
                                                  :value (token-text semicolon-token)
                                                  :span (token-span semicolon-token))))))
        (expect (cst-node->sexp cst :include-span t) :to-equal '(:type :binding
                        :value nil
                        :children ((:type :keyword
                                    :value "let"
                                    :children ()
                                    :span (:source "let answer = 42;"
                                           :start 0 :end 3
                                           :start-line 1 :start-column 1
                                           :end-line 1 :end-column 4))
                                   (:type :identifier
                                    :value "answer"
                                    :children ()
                                    :span (:source "let answer = 42;"
                                           :start 4 :end 10
                                           :start-line 1 :start-column 5
                                           :end-line 1 :end-column 11))
                                   (:type :punctuation
                                    :value "="
                                    :children ()
                                    :span (:source "let answer = 42;"
                                           :start 11 :end 12
                                           :start-line 1 :start-column 12
                                           :end-line 1 :end-column 13))
                                   (:type :number
                                    :value "42"
                                    :children ()
                                    :span (:source "let answer = 42;"
                                           :start 13 :end 15
                                           :start-line 1 :start-column 14
                                           :end-line 1 :end-column 16))
                                   (:type :punctuation
                                    :value ";"
                                    :children ()
                                    :span (:source "let answer = 42;"
                                           :start 15 :end 16
                                           :start-line 1 :start-column 16
                                           :end-line 1 :end-column 17)))
                        :span (:source "let answer = 42;"
                               :start 0 :end 16
                               :start-line 1 :start-column 1
                               :end-line 1 :end-column 17)))))))
 (readme-failure-rendering-snippet-test
  (with-pratt-diagnostic-context (tokenizer table)
    (assert-pratt-failure-rendering ("1 + +" tokenizer table)
      '("Expected PREFIX" "1 + +" "^"))))
 (readme-manual-diagnostic-snippet-test
  (assert-diagnostic-sample-rendering)))
