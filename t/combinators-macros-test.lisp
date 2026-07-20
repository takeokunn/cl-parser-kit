(in-package :cl-parser-kit/test)

;;; A self-recursive grammar defined with DEFPARSER, exercised below. A
;;; balanced-parenthesis wrapper around a number: 5, (5), ((5)), ...
(defparser %macros-nested-number ()
  (alt (type-token-value :number)
       (between (type-token :lparen)
                (%macros-nested-number)
                (type-token :rparen))))

(defun %paren-wrapped-number-tokens (depth)
  "DEPTH opening parens, a 5, then DEPTH closing parens."
  (coerce (append (loop repeat depth collect (make-token :type :lparen :text "("))
                  (list (make-token :type :number :text "5" :value 5))
                  (loop repeat depth collect (make-token :type :rparen :text ")")))
          'vector))

;;; PARSE-LET* ----------------------------------------------------------------

(it-sequential "combinator-parse-let-sequences-and-builds-value-test"
  (let ((tokens (vector (make-token :type :number :text "1" :value 1)
                        (make-token :type :plus :text "+")
                        (make-token :type :number :text "2" :value 2)))
        (parser (parse-let* ((a (type-token-value :number))
                             (_ (type-token :plus))
                             (b (type-token-value :number)))
                  (list :sum a b))))
    (assert-combinator-success (parse-tokens parser tokens)
        (value next failure)
      (expect next :to-equal 3)
      (expect value :to-equal '(:sum 1 2)))))

(it-sequential "combinator-parse-let-empty-bindings-returns-body-test"
  (let ((parser (parse-let* () (+ 40 2))))
    (assert-combinator-success (parse-tokens parser #())
        (value next failure)
      (expect next :to-equal 0)
      (expect value :to-equal 42))))

(it-sequential "combinator-parse-let-propagates-committed-failure-test"
  ;; Once the first binding consumes :PLUS, a later failing binding stays
  ;; committed -- PARSE-LET* is exactly nested BIND-PARSER.
  (let* ((tokens (vector (make-token :type :plus :text "+")))
         (parser (parse-let* ((a (type-token :plus))
                              (b (type-token :number)))
                   (list a b))))
    (assert-combinator-failure (parse-tokens parser tokens)
        (value next failure)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-committed-p failure) :to-be-truthy)
      (expect (parse-failure-expected failure) :to-equal :number))))

;;; PARSER-LAZY ---------------------------------------------------------------

(it-sequential "combinator-parser-lazy-supports-recursive-grammar-test"
  (let ((expr nil))
    (setf expr (alt (type-token-value :number)
                    (between (type-token :lparen)
                             (parser-lazy expr)
                             (type-token :rparen))))
    (assert-combinator-success (parse-tokens expr (%paren-wrapped-number-tokens 3))
        (value next failure)
      (expect next :to-equal 7)
      (expect value :to-equal 5))))

;;; DEFPARSER -----------------------------------------------------------------

(it-sequential "combinator-defparser-defines-recursive-parser-test"
  (assert-combinator-success
      (parse-tokens (%macros-nested-number) (%paren-wrapped-number-tokens 2))
      (value next failure)
    (expect next :to-equal 5)
    (expect value :to-equal 5)))

(it-sequential "combinator-defparser-parses-bare-base-case-test"
  (assert-combinator-success
      (parse-tokens (%macros-nested-number) (%paren-wrapped-number-tokens 0))
      (value next failure)
    (expect next :to-equal 1)
    (expect value :to-equal 5)))
