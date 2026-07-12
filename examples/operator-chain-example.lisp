(in-package :cl-user)

;; Example chain combinator setup for left- and right-associative operators.

(defparameter *operator-chain-tokenizer*
  (cl-parser-kit:make-tokenizer
   :rules (list (cl-parser-kit:make-whitespace-rule :skip-p t)
                (cl-parser-kit:make-literal-rule :minus "-")
                (cl-parser-kit:make-literal-rule :caret "^")
                (cl-parser-kit:make-number-rule))))

(defun parse-left-associative-chain-example (&optional (source "10 - 3 - 2"))
  (cl-parser-kit:parse-source
   (cl-parser-kit:chainl1
    (cl-parser-kit:map-parser
     (cl-parser-kit:type-token :number)
     #'cl-parser-kit:token-value)
    (cl-parser-kit:operator-parser
     (cl-parser-kit:literal "-" :type :minus)
     (lambda (left right)
       (- left right))))
   source
   *operator-chain-tokenizer*))

(defun parse-right-associative-chain-example (&optional (source "2 ^ 3 ^ 2"))
  (cl-parser-kit:parse-source
   (cl-parser-kit:chainr1
    (cl-parser-kit:map-parser
     (cl-parser-kit:type-token :number)
     #'cl-parser-kit:token-value)
    (cl-parser-kit:operator-parser
     (cl-parser-kit:literal "^" :type :caret)
     (lambda (left right)
       (expt left right))))
   source
   *operator-chain-tokenizer*))

;; (parse-left-associative-chain-example)
;; (parse-right-associative-chain-example)
