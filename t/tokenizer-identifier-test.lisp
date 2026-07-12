(in-package :cl-parser-kit/test)

(deftest-case tokenizer-identifier-rule-supports-custom-start-and-continue-predicates-test
  (let* ((tokenizer (%make-custom-identifier-tokenizer))
         (tokens (tokenize-string "$value tail? plain" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :identifier :value "$value")
           (%make-tokenizer-token-spec :type :identifier :value "tail?")
           (%make-tokenizer-token-spec :type :identifier :value "plain")))))

(deftest-case tokenizer-rejects-non-advancing-rules-test
  (let* ((rule (make-token-rule
                :type :stuck
                :matcher (lambda (source index)
                           (declare (ignore source index))
                           (values t 0 "" nil))))
         (tokenizer (make-tokenizer :rules (list rule))))
    (assert-signals error
      (tokenize "x" tokenizer))))

(deftest-case tokenizer-public-accessor-contract-test
  (let* ((calls '())
         (rule (make-token-rule
                :type :word
                :skip-p t
                :matcher (lambda (source index)
                           (push (list source index) calls)
                           (values t 2 "ab" "AB"))))
         (tokenizer (make-tokenizer :rules (list rule))))
    (assert-true (typep rule 'token-rule))
    (assert-equal :word (token-rule-type rule))
    (assert-true (token-rule-skip-p rule))
    (assert-equal (list rule) (tokenizer-rules tokenizer))
    (multiple-value-bind (matched consumed text value)
        (funcall (token-rule-matcher rule) "abc" 0)
      (assert-true matched)
      (assert-equal 2 consumed)
      (assert-equal "ab" text)
      (assert-equal "AB" value))
    (multiple-value-bind (matched consumed text value)
        (funcall (token-rule-matcher rule) "abc" 1)
      (assert-true matched)
      (assert-equal 2 consumed)
      (assert-equal "ab" text)
      (assert-equal "AB" value))
    (assert-equal '(("abc" 1) ("abc" 0))
                  calls)))
