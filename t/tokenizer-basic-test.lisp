(in-package :cl-parser-kit/test)

(it-sequential "tokenizer-basic"
  (let* ((tokenizer (make-tokenizer :rules (%basic-tokenizer-rules)))
         (tokens (tokenize-string "sum + 42" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :identifier :value "sum")
           (%make-tokenizer-token-spec :type :plus :value "+")
           (%make-tokenizer-token-spec :type :number :value 42)))))

(it-sequential "tokenizer-unknown-and-span-test"
  (let* ((tokenizer (make-tokenizer :rules (%number-only-tokenizer-rules)))
         (tokens (tokenize "1
?" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :number :value 1)
           (%make-tokenizer-token-spec :type :unknown
                                       :value #\?
                                       :text "?"
                                       :span '(2 1 2 2))))))
