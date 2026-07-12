(in-package :cl-user)

;; Parse a pre-tokenized stream and render a trailing-token failure.

(defun render-trailing-token-example ()
  (let* ((tokenizer (cl-parser-kit:make-tokenizer
                     :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                                  (cl-parser-kit:make-literal-rule :equals "=")
                                  (cl-parser-kit:make-literal-rule :semicolon ";")
                                  (cl-parser-kit:make-number-rule)
                                  (cl-parser-kit:make-identifier-rule))))
         (tokens (cl-parser-kit:tokenize-string "answer = 42 extra" tokenizer))
         (parser (cl-parser-kit:seq
                  (cl-parser-kit:type-token :identifier)
                  (cl-parser-kit:literal "=" :type :equals)
                  (cl-parser-kit:type-token :number))))
    (multiple-value-bind (ok value next failure)
        (cl-parser-kit:parse-all parser tokens)
      (declare (ignore value next))
      (if ok
          :ok
          (cl-parser-kit:parse-failure->string failure)))))

;; (render-trailing-token-example)
