(in-package :cl-parser-kit/test)

(it-sequential "tokenizer-identifier-rule-supports-custom-start-and-continue-predicates-test"
  (let* ((tokenizer (%make-custom-identifier-tokenizer))
         (tokens (tokenize-string "$value tail? plain" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :identifier :value "$value")
           (%make-tokenizer-token-spec :type :identifier :value "tail?")
           (%make-tokenizer-token-spec :type :identifier :value "plain")))))

(it-sequential "tokenizer-rejects-non-advancing-rules-test"
  (let* ((rule (make-token-rule
                :type :stuck
                :matcher (lambda (source index)
                           (declare (ignore source index))
                           (values t 0 "" nil))))
         (tokenizer (make-tokenizer :rules (list rule))))
    (expect (lambda () (tokenize "x" tokenizer)) :to-throw (quote error))))

(it-sequential "tokenizer-public-accessor-contract-test"
  (let* ((calls '())
         (rule (make-token-rule
                :type :word
                :skip-p t
                :matcher (lambda (source index)
                           (push (list source index) calls)
                           (values t 2 "ab" "AB"))))
         (tokenizer (make-tokenizer :rules (list rule))))
    (expect (typep rule 'token-rule) :to-be-truthy)
    (expect (token-rule-type rule) :to-equal :word)
    (expect (token-rule-skip-p rule) :to-be-truthy)
    (expect (tokenizer-rules tokenizer) :to-equal (list rule))
    (multiple-value-bind (matched consumed text value)
        (funcall (token-rule-matcher rule) "abc" 0)
      (expect matched :to-be-truthy)
      (expect consumed :to-equal 2)
      (expect text :to-equal "ab")
      (expect value :to-equal "AB"))
    (multiple-value-bind (matched consumed text value)
        (funcall (token-rule-matcher rule) "abc" 1)
      (expect matched :to-be-truthy)
      (expect consumed :to-equal 2)
      (expect text :to-equal "ab")
      (expect value :to-equal "AB"))
    (expect calls :to-equal '(("abc" 1) ("abc" 0)))))
