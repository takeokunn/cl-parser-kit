(in-package :cl-parser-kit/test)

(register-example-test-cases
 (readme-cst-snippet-test
  (let* ((span (make-span :start 0 :end 3))
         (cst (make-cst-node
               :type :binding
               :children (list (make-cst-node
                                :type :identifier
                                :value "answer"
                                :span span)))))
    (expect (cst-node->sexp cst :include-span t) :to-equal '(:type :binding
                    :value nil
                    :children ((:type :identifier
                                :value "answer"
                                :children ()
                                :span (:source nil
                                       :start 0 :end 3
                                       :start-line 1 :start-column 1
                                       :end-line 1 :end-column 1)))
                    :span nil))))
 (readme-lookahead-not-followed-by-snippet-test
  (let* ((tokens (vector (make-token :type :identifier :text "foo")
                         (make-token :type :plus :text "+")))
         (parser (seq
                  (lookahead
                   (seq (type-token :identifier)
                        (type-token :plus)))
                  (type-token :identifier)
                  (not-followed-by (type-token :identifier))
                  (type-token :plus))))
    (assert-example-successes
     ((parse-tokens parser tokens)
      (value next failure)
      (expect next :to-equal 2)
      (expect (length value) :to-equal 4)
      (expect (length (first value)) :to-equal 2)
      (expect (token-text (second value)) :to-equal "foo")
      (expect (token-text (fourth value)) :to-equal "+")))))
 (readme-operator-chain-snippet-test
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
      (expect next :to-equal 5)
      (expect value :to-equal 5))
     ((parse-source power-parser "2 ^ 3 ^ 2" tokenizer)
      (value next failure)
      (expect next :to-equal 5)
      (expect value :to-equal 512))))))

(assert-example-shape-failure-snippet)
