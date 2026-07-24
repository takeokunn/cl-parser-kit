(in-package :cl-parser-kit/test)

(defun %expr-num (n) (make-token :type :number :text (princ-to-string n) :value n))
(defun %expr-op (text) (make-token :type :op :text text))

(defun %arith-parser ()
  "* and / (tighter) then + and -, with unary prefix minus tightest of all."
  (make-expression-parser
   (type-token-value :number)
   (list (list (list :prefix (operator-parser (literal "-") (lambda (x) (- x)))))
         (list (list :infix-left (operator-parser (literal "*") #'*))
               (list :infix-left (operator-parser (literal "/") #'/)))
         (list (list :infix-left (operator-parser (literal "+") #'+))
               (list :infix-left (operator-parser (literal "-") #'-))))))

;;; MAKE-EXPRESSION-PARSER ----------------------------------------------------

(it-sequential "expression-parser-respects-precedence-test"
  ;; 1 + 2 * 3 -> 1 + 6 = 7
  (assert-combinator-success
      (parse-all (%arith-parser)
                 (vector (%expr-num 1) (%expr-op "+") (%expr-num 2) (%expr-op "*") (%expr-num 3)))
      (value next failure)
    (expect value :to-equal 7)))

(it-sequential "expression-parser-left-associates-test"
  ;; 1 - 2 - 3 -> (1 - 2) - 3 = -4
  (assert-combinator-success
      (parse-all (%arith-parser)
                 (vector (%expr-num 1) (%expr-op "-") (%expr-num 2) (%expr-op "-") (%expr-num 3)))
      (value next failure)
    (expect value :to-equal -4)))

(it-sequential "expression-parser-applies-prefix-test"
  ;; - 2 * 3 -> (-2) * 3 = -6 (prefix binds tighter than *)
  (assert-combinator-success
      (parse-all (%arith-parser)
                 (vector (%expr-op "-") (%expr-num 2) (%expr-op "*") (%expr-num 3)))
      (value next failure)
    (expect value :to-equal -6)))

(it-sequential "expression-parser-right-associates-test"
  ;; 2 ^ 3 ^ 2 -> 2 ^ (3 ^ 2) = 512
  (let ((parser (make-expression-parser
                 (type-token-value :number)
                 (list (list (list :infix-right (operator-parser (literal "^") #'expt)))))))
    (assert-combinator-success
        (parse-all parser
                   (vector (%expr-num 2) (%expr-op "^") (%expr-num 3) (%expr-op "^") (%expr-num 2)))
        (value next failure)
      (expect value :to-equal 512))))

(it-sequential "expression-parser-applies-postfix-test"
  (let ((parser (make-expression-parser
                 (type-token-value :number)
                 (list (list (list :postfix (operator-parser (literal "!")
                                                             (lambda (x) (list :fact x)))))))))
    (assert-combinator-success
        (parse-all parser (vector (%expr-num 5) (%expr-op "!") (%expr-op "!")))
        (value next failure)
      (expect value :to-equal '(:fact (:fact 5))))))

(it-sequential "expression-parser-non-assoc-accepts-single-test"
  (let ((parser (make-expression-parser
                 (type-token-value :number)
                 (list (list (list :infix-non-assoc
                                   (operator-parser (literal "<") (lambda (l r) (list :< l r)))))))))
    (assert-combinator-success
        (parse-all parser (vector (%expr-num 1) (%expr-op "<") (%expr-num 2)))
        (value next failure)
      (expect value :to-equal '(:< 1 2)))))

(it-sequential "expression-parser-non-assoc-rejects-chain-test"
  (let ((parser (make-expression-parser
                 (type-token-value :number)
                 (list (list (list :infix-non-assoc
                                   (operator-parser (literal "<") (lambda (l r) (list :< l r)))))))))
    (assert-combinator-failure
        (parse-all parser
                   (vector (%expr-num 1) (%expr-op "<") (%expr-num 2) (%expr-op "<") (%expr-num 3)))
        (value next failure))))

(it-sequential "expression-parser-rejects-mixed-associativity-level-test"
  (expect (nth-value 1 (ignore-errors
                        (make-expression-parser
                         (type-token-value :number)
                         (list (list (list :infix-left (operator-parser (literal "+") #'+))
                                     (list :infix-right (operator-parser (literal "^") #'expt)))))))
          :to-be-truthy))

(it-sequential "expression-parser-rejects-excessive-table-test"
  (let ((*maximum-parser-repetition-count* 1))
    (expect (lambda ()
              (make-expression-parser
               (type-token-value :number)
               (list (list (list :infix-left (operator-parser (literal "+") #'+)))
                     (list (list :infix-left (operator-parser (literal "-") #'-))))))
            :to-throw 'error)))

(it-sequential "expression-parser-rejects-excessive-level-test"
  (let ((*maximum-parser-repetition-count* 1))
    (expect (lambda ()
              (make-expression-parser
               (type-token-value :number)
               (list (list (list :infix-left (operator-parser (literal "+") #'+))
                           (list :infix-left (operator-parser (literal "-") #'-))))))
            :to-throw 'error)))

;;; SEQUENCE-OF ---------------------------------------------------------------

(it-sequential "combinator-sequence-of-runs-list-in-order-test"
  (with-combinator-tokens (tokens '((:type :a :text "a") (:type :b :text "b")))
    (let ((parser (sequence-of (list (type-token :a) (type-token :b)))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect (mapcar #'token-type value) :to-equal '(:a :b))))))

;;; CHAIN-POSTFIX -------------------------------------------------------------

(it-sequential "combinator-chain-postfix-folds-suffixes-test"
  (let ((parser (chain-postfix (type-token-value :number)
                               (map-parser (literal "!")
                                           (lambda (_op)
                                             (declare (ignore _op))
                                             (lambda (v) (list :fact v)))))))
    (assert-combinator-success
        (parse-tokens parser (vector (%expr-num 5) (%expr-op "!") (%expr-op "!")))
        (value next failure)
      (expect next :to-equal 3)
      (expect value :to-equal '(:fact (:fact 5))))))

(it-sequential "combinator-chain-postfix-with-no-suffix-yields-base-test"
  (let ((parser (chain-postfix (type-token-value :number)
                               (map-parser (literal "!")
                                           (lambda (_op)
                                             (declare (ignore _op))
                                             (lambda (v) (list :fact v)))))))
    (assert-combinator-success
        (parse-tokens parser (vector (%expr-num 5)))
        (value next failure)
      (expect next :to-equal 1)
      (expect value :to-equal 5))))
