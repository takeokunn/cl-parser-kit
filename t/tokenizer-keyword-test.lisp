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

(it-sequential "tokenizer-keyword-rule-declines-when-flanked-by-a-preceding-digit-test"
  ;; MAKE-KEYWORD-RULE's leading-flank check must decline "let" when it is
  ;; immediately preceded by an identifier character even when that character
  ;; came from an entirely different token (a NUMBER here, not another
  ;; identifier) -- the check only looks at the raw preceding SOURCE
  ;; character, not at what rule produced it.
  (let* ((tokenizer (make-tokenizer :rules (list (make-number-rule)
                                                 (make-keyword-rule :let "let")
                                                 (make-identifier-rule))))
         (tokens (tokenize-string "2let" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :number :value 2)
           (%make-tokenizer-token-spec :type :identifier :value "let")))))

(it-sequential "tokenizer-keyword-rule-rejects-empty-keyword-test"
  (expect (lambda () (make-keyword-rule :empty ""))
          :to-throw 'error))

(it-sequential "tokenizer-keyword-rule-rejects-a-non-string-keyword-test"
  (expect (lambda () (make-keyword-rule :bad 42))
          :to-throw 'error))

(it-sequential "tokenizer-keyword-rule-is-case-sensitive-by-default-test"
  ;; CASE-SENSITIVE defaults to T: "Let" must not match the :LET keyword and
  ;; falls through to the identifier rule instead.
  (let* ((tokenizer (%make-let-keyword-tokenizer))
         (tokens (tokenize-string "Let" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :identifier :value "Let")))))
