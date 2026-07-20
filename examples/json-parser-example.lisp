(in-package :cl-user)

;; A complete, recursive JSON parser built on cl-parser-kit -- an end-to-end
;; demonstration exercising the tokenizer (escaped strings, signed/exponent
;; numbers, keyword and literal rules), a recursive grammar (via DEFPARSER), and
;; the sequence/choice/separator combinators. Objects decode to alists, arrays to
;; lists, and true/false/null to T / NIL / :NULL; strings and numbers decode to
;; their Lisp values.

(defparameter *json-tokenizer*
  (cl-parser-kit:make-tokenizer
   :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                (cl-parser-kit:make-string-rule
                 :type :string :escape-char #\\
                 :escapes (list (cons #\n #\Newline) (cons #\t #\Tab)
                                (cons #\r #\Return) (cons #\" #\")
                                (cons #\\ #\\) (cons #\/ #\/)))
                (cl-parser-kit:make-float-rule
                 :type :number :allow-sign t :require-fractional nil)
                (cl-parser-kit:make-keyword-rule :true "true")
                (cl-parser-kit:make-keyword-rule :false "false")
                (cl-parser-kit:make-keyword-rule :null "null")
                (cl-parser-kit:make-literal-rule :lbrace "{")
                (cl-parser-kit:make-literal-rule :rbrace "}")
                (cl-parser-kit:make-literal-rule :lbracket "[")
                (cl-parser-kit:make-literal-rule :rbracket "]")
                (cl-parser-kit:make-literal-rule :colon ":")
                (cl-parser-kit:make-literal-rule :comma ","))))

;; DEFPARSER wraps each body in a lazy parser, so JSON-VALUE can refer to itself
;; (through JSON-ARRAY / JSON-OBJECT) without looping at build time.
(cl-parser-kit:defparser json-value ()
  (cl-parser-kit:alt (cl-parser-kit:type-token-value :string)
                     (cl-parser-kit:type-token-value :number)
                     (cl-parser-kit:as-value t (cl-parser-kit:type-token :true))
                     (cl-parser-kit:as-value nil (cl-parser-kit:type-token :false))
                     (cl-parser-kit:as-value :null (cl-parser-kit:type-token :null))
                     (json-array)
                     (json-object)))

(cl-parser-kit:defparser json-array ()
  (cl-parser-kit:between
   (cl-parser-kit:type-token :lbracket)
   (cl-parser-kit:sep-by (json-value) (cl-parser-kit:type-token :comma))
   (cl-parser-kit:type-token :rbracket)))

(cl-parser-kit:defparser json-pair ()
  (cl-parser-kit:seq-map (lambda (key colon value)
                           (declare (ignore colon))
                           (cons key value))
                         (cl-parser-kit:type-token-value :string)
                         (cl-parser-kit:type-token :colon)
                         (json-value)))

(cl-parser-kit:defparser json-object ()
  (cl-parser-kit:between
   (cl-parser-kit:type-token :lbrace)
   (cl-parser-kit:sep-by (json-pair) (cl-parser-kit:type-token :comma))
   (cl-parser-kit:type-token :rbrace)))

(defun parse-json (source)
  "Tokenize and parse SOURCE as JSON, returning the standard
(values ok decoded-value next failure)."
  (cl-parser-kit:parse-source (json-value) source *json-tokenizer*))

(defun parse-json-example ()
  "Parse a small nested JSON document, decoding it to Lisp values."
  (parse-json "{\"ok\": true, \"vals\": [1, 2.5], \"tag\": null}"))

;; (parse-json-example)
;; => T, (("ok" . T) ("vals" 1.0d0 2.5d0) ("tag" . :NULL)), 17, NIL
