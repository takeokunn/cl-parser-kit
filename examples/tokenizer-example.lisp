(in-package :cl-user)

;; Load cl-parser-kit, then evaluate the form below.

(defun tokenize-sum-example ()
  (let* ((tokenizer (cl-parser-kit:make-tokenizer
                     :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                                  (cl-parser-kit:make-literal-rule :plus "+")
                                  (cl-parser-kit:make-number-rule)
                                  (cl-parser-kit:make-identifier-rule))))
         (tokens (cl-parser-kit:tokenize "sum + 42" tokenizer)))
    tokens))

(defun tokenize-custom-language-example ()
  (let* ((identifier-char-p
           (lambda (char)
             (or (alpha-char-p char)
                 (digit-char-p char)
                 (char= char #\_)
                 (char= char #\$)
                 (char= char #\?))))
         (tokenizer
           (cl-parser-kit:make-tokenizer
            :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                         (cl-parser-kit:make-line-comment-rule :skip-p t)
                         (cl-parser-kit:make-block-comment-rule :skip-p t)
                         (cl-parser-kit:make-string-rule :escape-char #\\)
                         (cl-parser-kit:make-keyword-rule
                          :if "if"
                          :identifier-char-predicate identifier-char-p)
                         (cl-parser-kit:make-identifier-rule
                          :start-predicate identifier-char-p
                          :continue-predicate identifier-char-p))))
         (tokens (cl-parser-kit:tokenize
                  "if $value /* note */ \"ok\" ; trailing comment
if?"
                  tokenizer)))
    tokens))

;; Evaluate this form in the REPL to inspect the token stream.
;; (tokenize-sum-example)
;; (tokenize-custom-language-example)
