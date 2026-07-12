(in-package :cl-user)

;; Example parser for `let (x, y, z);` using practical sequence helpers.

(defparameter *combinator-tokenizer*
  (cl-parser-kit:make-tokenizer
   :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                (cl-parser-kit:make-keyword-rule :let "let")
                (cl-parser-kit:make-literal-rule :lparen "(")
                (cl-parser-kit:make-literal-rule :rparen ")")
                (cl-parser-kit:make-literal-rule :comma ",")
                (cl-parser-kit:make-literal-rule :semicolon ";")
                (cl-parser-kit:make-identifier-rule))))

(defparameter *identifier-list-parser*
  (cl-parser-kit:delimited-sep-by1
   (cl-parser-kit:literal "(" :type :lparen)
   (cl-parser-kit:type-token :identifier)
   (cl-parser-kit:literal "," :type :comma)
   (cl-parser-kit:literal ")" :type :rparen)))

(defparameter *let-list-parser*
  (cl-parser-kit:seq
   (cl-parser-kit:preceded-by
    (cl-parser-kit:literal "let" :type :let)
    *identifier-list-parser*)
   (cl-parser-kit:opt (cl-parser-kit:literal ";" :type :semicolon))
   (cl-parser-kit:end-of-input)))

;; Evaluate this form in the REPL to parse a grouped binding list.
(defun parse-let-list-example (&optional (source "let (x, y, z);"))
  (cl-parser-kit:parse-source *let-list-parser* source *combinator-tokenizer*))

;; (parse-let-list-example)
