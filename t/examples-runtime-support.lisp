(in-package :cl-parser-kit/test)

(defun dsl-identifier-char-p (char)
  (or (alpha-char-p char)
      (digit-char-p char)
      (char= char #\_)
      (char= char #\$)
      (char= char #\?)))

(defun make-dsl-snippet-tokenizer ()
  (make-tokenizer
   :rules (list (make-whitespace-rule :skip-p t)
                (make-line-comment-rule :skip-p t)
                (make-block-comment-rule :skip-p t)
                (make-string-rule :escape-char #\\)
                (make-keyword-rule
                 :if "if"
                 :identifier-char-predicate #'dsl-identifier-char-p)
                (make-identifier-rule
                 :start-predicate #'dsl-identifier-char-p
                 :continue-predicate #'dsl-identifier-char-p))))

(defun make-let-example-tokenizer ()
  (make-tokenizer
   :rules (list (make-whitespace-rule :skip-p t)
                (make-keyword-rule :let "let")
                (make-literal-rule :equals "=")
                (make-literal-rule :lparen "(")
                (make-literal-rule :rparen ")")
                (make-literal-rule :comma ",")
                (make-literal-rule :semicolon ";")
                (make-number-rule)
                (make-identifier-rule))))

(defun make-expression-example-tokenizer ()
  (make-tokenizer
   :rules (list (make-whitespace-rule :skip-p t)
                (make-literal-rule :plus "+")
                (make-number-rule)
                (make-identifier-rule))))

(defun make-punctuated-example-tokenizer ()
  (make-tokenizer
   :rules (list (make-whitespace-rule :skip-p t)
                (make-literal-rule :equals "=")
                (make-literal-rule :lparen "(")
                (make-literal-rule :rparen ")")
                (make-literal-rule :comma ",")
                (make-literal-rule :semicolon ";")
                (make-number-rule)
                (make-identifier-rule))))

(defun make-operator-chain-tokenizer ()
  (make-tokenizer
   :rules (list (make-whitespace-rule :skip-p t)
                (make-literal-rule :minus "-")
                (make-literal-rule :caret "^")
                (make-number-rule))))

(defun tokenize-dsl-sample-source ()
  (tokenize-string *dsl-sample-source* (make-dsl-snippet-tokenizer)))

(defun assert-dsl-sample-tokens (&optional tokens)
  (let ((tokens (or tokens (tokenize-dsl-sample-source))))
    (assert-equal 4 (length tokens))
    (assert-equal :if (token-type (aref tokens 0)))
    (assert-equal "$value" (token-value (aref tokens 1)))
    (assert-equal :string (token-type (aref tokens 2)))
    (assert-equal "ok" (token-value (aref tokens 2)))
    (assert-equal :identifier (token-type (aref tokens 3)))
    (assert-equal "if?" (token-value (aref tokens 3)))))

(defun make-diagnostic-sample ()
  (error-diagnostic
   "bad token"
   :span (make-span :source *diagnostic-sample-source*
                    :start 0 :end 3
                    :start-line 1 :start-column 1
                    :end-line 1 :end-column 2)
   :notes (list (note-diagnostic
                 "check syntax"
                 :span (make-span :start 4 :end 5
                                  :start-line 1 :start-column 5
                                  :end-line 1 :end-column 6)))
   :fixes (list (make-fix-it
                 :span (make-span :start 0 :end 1)
                 :replacement "x"))))

(defun render-diagnostic-sample ()
  (diagnostic->string (make-diagnostic-sample)))

(defun assert-diagnostic-sample-rendering (&optional (rendered (render-diagnostic-sample)))
  (assert-true (stringp rendered))
  (assert-string-contains-all
   rendered
   '("bad token"
     "foo + bar"
     "note: check syntax [1:5-1:6]"
     "fix-it [1:1-1:1]: replace with \"x\"")))
