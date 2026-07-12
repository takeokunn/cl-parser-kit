(in-package :cl-parser-kit/test)

(deftest-case tokenizer-line-comment-rule-stops-at-carriage-return-test
  (let* ((skip-tokenizer (make-tokenizer :rules (%skip-line-comment-tokenizer-rules)))
         (visible-tokenizer
           (%make-line-comment-tokenizer :skip-p nil :value-function #'length))
         (skip-tokens (tokenize (format nil "foo ; note~C42" #\Return)
                                skip-tokenizer))
         (visible-tokens (tokenize (format nil "; note~C42" #\Return)
                                   visible-tokenizer)))
    (assert-tokenizer-tokens
     skip-tokens
     (list (%make-tokenizer-token-spec :type :identifier :value "foo")
           (%make-tokenizer-token-spec :type :number
                                       :value 42
                                       :span '(2 1 2 3))))
    (assert-tokenizer-tokens
     visible-tokens
     (list (%make-tokenizer-token-spec :type :comment
                                       :value 6
                                       :span '(1 1 1 7))
           (%make-tokenizer-token-spec :type :number
                                       :value 42
                                       :span '(2 1 2 3))))))

(deftest-case tokenizer-whitespace-rule-trims-page-characters-test
  (let* ((tokenizer (make-tokenizer
                     :rules (list (make-whitespace-rule :skip-p nil)
                                  (make-number-rule))))
         (tokens (tokenize (format nil "~C42" #\Page) tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :whitespace
                                       :text (string #\Page)
                                       :value "")
           (%make-tokenizer-token-spec :type :number
                                       :value 42
                                       :span '(1 2 1 4))))))
