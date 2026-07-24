(in-package :cl-parser-kit/test)

(it-sequential "tokenizer-non-skipping-comment-rules-test"
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

(it-sequential "tokenizer-unterminated-line-comment-consumes-to-end-test"
  ;; A line comment with no trailing line break must consume the rest of the
  ;; source (%LINE-COMMENT-END's OR fallback) rather than something shorter.
  (let* ((tokenizer (%make-line-comment-tokenizer :skip-p nil
                                                  :value-function #'identity))
         (tokens (tokenize "; unterminated" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :comment :value "; unterminated")))))

(it-sequential "tokenizer-unterminated-block-comment-consumes-to-end-test"
  ;; A block comment with no closing delimiter must consume the rest of the
  ;; source instead of crashing the tokenizer on untrusted input.
  (let* ((tokenizer (%make-block-comment-tokenizer :skip-p nil
                                                   :value-function #'identity))
         (tokens (tokenize "/* unclosed" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :comment :value "/* unclosed")))))

(it-sequential "tokenizer-block-comment-rule-matcher-declines-when-prefix-does-not-fit-test"
  ;; TOKEN-RULE-MATCHER is public API; %MAKE-PREFIXED-COMMENT-RULE's
  ;; prefix-fits-in-remaining-source guard must decline (not index out of
  ;; bounds) when fewer characters remain than the prefix needs, even when the
  ;; remaining text is itself a genuine prefix of the delimiter.
  (let ((rule (make-block-comment-rule :start "/*" :end "*/")))
    (expect (funcall (token-rule-matcher rule) "a/" 1) :to-be-falsy)))

(it-sequential "tokenizer-nested-block-comment-unterminated-consumes-to-end-test"
  ;; An unterminated nested comment (even one that opens a further nested
  ;; START) must consume the rest of the source instead of erroring --
  ;; %NESTED-BLOCK-COMMENT-END's off-the-end guard, mirroring
  ;; %BLOCK-COMMENT-END's non-nested counterpart.
  (let* ((source "/* a /* unterminated")
         (tokens (%tokenize-with (list (make-nested-block-comment-rule
                                        :type :comment :skip-p nil))
                                 source)))
    (expect (length tokens) :to-equal 1)
    (expect (token-text (elt tokens 0)) :to-equal source)))

(it-sequential "tokenizer-rule-ordering-test"
  (let* ((tokenizer (%make-arrow-tokenizer))
         (tokens (tokenize-string "-> target" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :arrow :value "->")
           (%make-tokenizer-token-spec :type :identifier :value "target")))))
