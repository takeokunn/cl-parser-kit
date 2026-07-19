(in-package :cl-parser-kit/test)

(it-sequential "tokenizer-keyword-rule-boundary-test"
  (let* ((tokenizer (%make-let-keyword-tokenizer))
         (tokens (tokenize-string "let lets outlet let2" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :let :value "let")
           (%make-tokenizer-token-spec :type :identifier :value "lets")
           (%make-tokenizer-token-spec :type :identifier :value "outlet")
           (%make-tokenizer-token-spec :type :identifier :value "let2")))))

(it-sequential "tokenizer-keyword-rule-punctuation-test"
  (let* ((tokenizer (%make-let-keyword-tokenizer :include-lparen-p t))
         (tokens (tokenize-string "let (value" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :let :value "let")
           (%make-tokenizer-token-spec :type :lparen :value "(")
           (%make-tokenizer-token-spec :type :identifier :value "value")))))

(it-sequential "tokenizer-keyword-rule-supports-custom-identifier-boundaries-test"
  (let* ((tokenizer (%make-custom-boundary-keyword-tokenizer))
         (tokens (tokenize-string "if if? maybe?" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :if :value "if")
           (%make-tokenizer-token-spec :type :identifier :value "if?")
           (%make-tokenizer-token-spec :type :identifier :value "maybe?")))))
