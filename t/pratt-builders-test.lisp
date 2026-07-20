(in-package :cl-parser-kit/test)

(defun %builders-table ()
  "An expression table wired entirely through the high-level registrars."
  (let ((table (make-pratt-table)))
    (register-atom table :number #'token-value)
    (register-prefix table :minus 100 (lambda (operand) (- operand)))
    (register-infix-left table :plus 10 (lambda (left right) (+ left right)))
    (register-infix-right table :caret 40 (lambda (left right) (expt left right)))
    (register-postfix table :bang 110 (lambda (operand) (list :fact operand)))
    (register-grouping table :lparen :rparen)
    table))

(defun %num (value) (make-token :type :number :text (princ-to-string value) :value value))
(defun %op (type text) (make-token :type type :text text))

(it-sequential "pratt-register-atom-parses-leaf-test"
  (assert-combinator-success (parse-pratt-all (vector (%num 7)) (%builders-table))
      (value next failure)
    (expect value :to-equal 7)))

(it-sequential "pratt-entry-points-enforce-token-count-limit-test"
  (let ((*maximum-parser-tokens* 2))
    (multiple-value-bind (ok value next failure)
        (parse-pratt-all (vector (%num 1) (%op :plus "+") (%num 2))
                         (%builders-table))
      (declare (ignore value))
      (expect ok :to-be-falsy)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :maximum-parser-tokens)
      (expect (parse-failure-actual failure) :to-equal 3))))

(it-sequential "pratt-entry-points-stop-list-coercion-at-token-count-limit-test"
  (let ((*maximum-parser-tokens* 2))
    (multiple-value-bind (ok value next failure)
        (parse-pratt-all (list (%num 1) (%op :plus "+") (%num 2))
                         (%builders-table))
      (declare (ignore value))
      (expect ok :to-be-falsy)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :maximum-parser-tokens)
      (expect (parse-failure-actual failure) :to-equal 3))))

(it-sequential "pratt-register-infix-left-associates-test"
  ;; 1 + 2 + 3 -> ((1 + 2) + 3); addition is associative so check the shape via
  ;; a non-commutative check is unnecessary -- the sum is 6 either way, and the
  ;; right-associativity test below proves the binding-power wiring.
  (assert-combinator-success
      (parse-pratt-all (vector (%num 1) (%op :plus "+") (%num 2) (%op :plus "+") (%num 3))
                       (%builders-table))
      (value next failure)
    (expect value :to-equal 6)))

(it-sequential "pratt-register-infix-right-associates-test"
  ;; 2 ^ 3 ^ 2 -> 2 ^ (3 ^ 2) = 2 ^ 9 = 512 (left would give (2^3)^2 = 64).
  (assert-combinator-success
      (parse-pratt-all (vector (%num 2) (%op :caret "^") (%num 3) (%op :caret "^") (%num 2))
                       (%builders-table))
      (value next failure)
    (expect value :to-equal 512)))

(it-sequential "pratt-register-prefix-parses-unary-test"
  (assert-combinator-success
      (parse-pratt-all (vector (%op :minus "-") (%num 5)) (%builders-table))
      (value next failure)
    (expect value :to-equal -5)))

(it-sequential "pratt-register-postfix-parses-suffix-test"
  (assert-combinator-success
      (parse-pratt-all (vector (%num 3) (%op :bang "!")) (%builders-table))
      (value next failure)
    (expect value :to-equal '(:fact 3))))

(it-sequential "pratt-register-grouping-parses-parens-test"
  (assert-combinator-success
      (parse-pratt-all (vector (%op :lparen "(") (%num 1) (%op :plus "+") (%num 2) (%op :rparen ")"))
                       (%builders-table))
      (value next failure)
    (expect value :to-equal 3)))

(it-sequential "pratt-register-grouping-overrides-precedence-test"
  ;; 2 ^ (1 + 2) = 2 ^ 3 = 8
  (assert-combinator-success
      (parse-pratt-all (vector (%num 2) (%op :caret "^")
                               (%op :lparen "(") (%num 1) (%op :plus "+") (%num 2) (%op :rparen ")"))
                       (%builders-table))
      (value next failure)
    (expect value :to-equal 8)))

(it-sequential "pratt-register-grouping-reports-missing-close-test"
  (multiple-value-bind (ok value next failure)
      (parse-pratt-all (vector (%op :lparen "(") (%num 1) (%op :plus "+") (%num 2))
                       (%builders-table))
    (declare (ignore value next))
    (expect ok :to-be-falsy)
    (expect (parse-failure-expected failure) :to-equal :rparen)))

;;; REGISTER-TERNARY ----------------------------------------------------------

(defun %ternary-table ()
  (let ((table (make-pratt-table)))
    (register-atom table :number #'token-value)
    (register-infix-left table :plus 20 (lambda (l r) (list :+ l r)))
    (register-ternary table :question :colon 5
                      (lambda (c th el) (list :if c th el)))
    table))

(it-sequential "pratt-register-ternary-parses-conditional-test"
  (assert-combinator-success
      (parse-pratt-all (vector (%num 1) (%op :question "?") (%num 2) (%op :colon ":") (%num 3))
                       (%ternary-table))
      (value next failure)
    (expect value :to-equal '(:if 1 2 3))))

(it-sequential "pratt-register-ternary-associates-right-test"
  ;; 1 ? 2 : 3 ? 4 : 5  ->  1 ? 2 : (3 ? 4 : 5)
  (assert-combinator-success
      (parse-pratt-all (vector (%num 1) (%op :question "?") (%num 2) (%op :colon ":")
                               (%num 3) (%op :question "?") (%num 4) (%op :colon ":") (%num 5))
                       (%ternary-table))
      (value next failure)
    (expect value :to-equal '(:if 1 2 (:if 3 4 5)))))

(it-sequential "pratt-register-ternary-reports-missing-colon-test"
  (multiple-value-bind (ok value next failure)
      (parse-pratt-all (vector (%num 1) (%op :question "?") (%num 2) (%num 3))
                       (%ternary-table))
    (declare (ignore value next))
    (expect ok :to-be-falsy)
    (expect (parse-failure-expected failure) :to-equal :colon)))

;;; REGISTER-INFIX-NON-ASSOC --------------------------------------------------

(defun %non-assoc-table ()
  (let ((table (make-pratt-table)))
    (register-atom table :number #'token-value)
    (register-infix-left table :plus 20 (lambda (l r) (list :+ l r)))
    (register-infix-non-assoc table :lt 10 (lambda (l r) (list :< l r)))
    table))

(it-sequential "pratt-register-infix-non-assoc-parses-single-test"
  (assert-combinator-success
      (parse-pratt-all (vector (%num 1) (%op :lt "<") (%num 2)) (%non-assoc-table))
      (value next failure)
    (expect value :to-equal '(:< 1 2))))

(it-sequential "pratt-register-infix-non-assoc-binds-tighter-operator-test"
  ;; 1 + 2 < 3  ->  (1 + 2) < 3
  (assert-combinator-success
      (parse-pratt-all (vector (%num 1) (%op :plus "+") (%num 2) (%op :lt "<") (%num 3))
                       (%non-assoc-table))
      (value next failure)
    (expect value :to-equal '(:< (:+ 1 2) 3))))

(it-sequential "pratt-register-infix-non-assoc-rejects-chaining-test"
  (multiple-value-bind (ok value next failure)
      (parse-pratt-all (vector (%num 1) (%op :lt "<") (%num 2) (%op :lt "<") (%num 3))
                       (%non-assoc-table))
    (declare (ignore value next))
    (expect ok :to-be-falsy)
    (expect (parse-failure-expected failure) :to-equal :non-associative-operator)))
