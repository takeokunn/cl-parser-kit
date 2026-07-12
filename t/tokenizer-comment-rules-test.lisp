(in-package :cl-parser-kit/test)

(deftest-case tokenizer-non-skipping-comment-rules-test
  (let* ((line-tokenizer
           (%make-line-comment-tokenizer
            :skip-p nil
            :value-function #'length))
         (block-tokenizer
           (%make-block-comment-tokenizer
            :skip-p nil
            :value-function #'length))
         (line-tokens (tokenize "; note
42" line-tokenizer))
         (block-tokens (tokenize "/* a
bc */ 42" block-tokenizer)))
    (assert-tokenizer-tokens
     line-tokens
     (list (%make-tokenizer-token-spec :type :comment :value 6)
           (%make-tokenizer-token-spec :type :number :value 42)))
    (assert-tokenizer-tokens
     block-tokens
     (list (%make-tokenizer-token-spec :type :comment
                                       :value 10
                                       :span '(1 1 2 6))
           (%make-tokenizer-token-spec :type :number :value 42)))))

(deftest-case tokenizer-rule-ordering-test
  (let* ((tokenizer (%make-arrow-tokenizer))
         (tokens (tokenize-string "-> target" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :arrow :value "->")
           (%make-tokenizer-token-spec :type :identifier :value "target")))))
