(in-package :cl-parser-kit/test)

(it-sequential "tokenizer-span-tracks-crlf-lines-test"
  (let* ((tokenizer (make-tokenizer :rules (%number-only-tokenizer-rules)))
         (tokens (tokenize (format nil "1~C~C?"
                                   #\Return
                                   #\Newline)
                           tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :number :value 1)
           (%make-tokenizer-token-spec :type :unknown
                                       :value #\?
                                       :text "?"
                                       :span '(2 1 2 2))))))

(it-sequential "tokenizer-comment-rules-test"
  (let* ((line-tokenizer (make-tokenizer :rules (%skip-line-comment-tokenizer-rules)))
         (block-tokenizer (make-tokenizer :rules (%skip-block-comment-tokenizer-rules)))
         (line-tokens (tokenize "foo ; ignore me
42" line-tokenizer))
         (block-tokens (tokenize "foo /* ignore me */ 42" block-tokenizer)))
    (assert-tokenizer-tokens
     line-tokens
     (list (%make-tokenizer-token-spec :type :identifier :value "foo")
           (%make-tokenizer-token-spec :type :number :value 42)))
    (assert-tokenizer-tokens
     block-tokens
     (list (%make-tokenizer-token-spec :type :identifier :value "foo")
           (%make-tokenizer-token-spec :type :number :value 42)))))
