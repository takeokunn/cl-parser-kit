(in-package :cl-user)

;; Pratt parsing example that also exposes structured failure diagnostics.
;; Try: (parse-expression-source "1 + +")

(defparameter *tokenizer*
  (cl-parser-kit:make-tokenizer
   :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                (cl-parser-kit:make-literal-rule :plus "+")
                (cl-parser-kit:make-number-rule))))

(defparameter *table*
  (let ((table (cl-parser-kit:make-pratt-table)))
    (labels ((number-nud (token stream next current-table)
               (declare (ignore stream current-table))
               (values t (cl-parser-kit:token-value token) next nil))
             (plus-led (left op right next current-table)
               (declare (ignore op current-table))
               (values t (list :add left right) next nil)))
      (cl-parser-kit:register-prefix-operator table :number 0 #'number-nud)
      (cl-parser-kit:register-infix-operator table :plus 10 11 #'plus-led))
    table))

(defun parse-expression-source (source &optional (tokenizer *tokenizer*) (table *table*))
  (multiple-value-bind (ok value next failure)
      (cl-parser-kit:parse-pratt-source source tokenizer table)
    (if ok
        (values t value next nil)
        (values nil
                (cl-parser-kit:parse-failure->string failure)
                next
                failure))))
