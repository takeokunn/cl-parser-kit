(in-package :cl-user)

;; Operator-precedence arithmetic on the combinator layer via
;; MAKE-EXPRESSION-PARSER. The table lists precedence levels highest first:
;; unary prefix `-` binds tightest, then `*`, then `+`. Parsing `1 + 2 * 3`
;; therefore yields 1 + (2 * 3) = 7. Contrast examples/expression-parser.lisp,
;; which builds the same shape of grammar with the token-keyed Pratt parser.

(defun parse-arithmetic-expression-example ()
  (let ((parser
          (cl-parser-kit:make-expression-parser
           (cl-parser-kit:type-token-value :number)
           (list (list (list :prefix
                             (cl-parser-kit:operator-parser
                              (cl-parser-kit:literal "-") (lambda (x) (- x)))))
                 (list (list :infix-left
                             (cl-parser-kit:operator-parser
                              (cl-parser-kit:literal "*") #'*)))
                 (list (list :infix-left
                             (cl-parser-kit:operator-parser
                              (cl-parser-kit:literal "+") #'+))))))
        (tokens (vector (cl-parser-kit:make-token :type :number :text "1" :value 1)
                        (cl-parser-kit:make-token :type :op :text "+")
                        (cl-parser-kit:make-token :type :number :text "2" :value 2)
                        (cl-parser-kit:make-token :type :op :text "*")
                        (cl-parser-kit:make-token :type :number :text "3" :value 3))))
    (cl-parser-kit:parse-all parser tokens)))

;; (parse-arithmetic-expression-example)
