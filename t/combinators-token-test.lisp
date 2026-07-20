(in-package :cl-parser-kit/test)

;;; ANY-TOKEN -----------------------------------------------------------------

(it-sequential "combinator-any-token-matches-any-single-token-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (any-token)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type value) :to-equal :identifier)))))

(it-sequential "combinator-any-token-fails-at-eof-test"
  (let ((parser (any-token)))
    (assert-combinator-failure (parse-tokens parser #())
        (value next failure)
      (expect (parse-failure-expected failure) :to-equal :any-token)
      (expect (parse-failure-actual failure) :to-equal :eof))))

;;; TOKEN-TYPE-IN -------------------------------------------------------------

(it-sequential "combinator-token-type-in-matches-member-test"
  (with-combinator-tokens (tokens '((:type :minus :text "-")))
    (let ((parser (token-type-in :plus :minus)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type value) :to-equal :minus)))))

(it-sequential "combinator-token-type-in-rejects-non-member-test"
  (with-combinator-tokens (tokens '((:type :star :text "*")))
    (let ((parser (token-type-in :plus :minus)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal '(:plus :minus))
        (expect (token-type (parse-failure-actual failure)) :to-equal :star)))))

;;; TOKEN-TEXT-IN -------------------------------------------------------------

(it-sequential "combinator-token-text-in-matches-member-lexeme-test"
  (with-combinator-tokens (tokens '((:type :keyword :text "while")))
    (let ((parser (token-text-in "if" "while" "for")))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-text value) :to-equal "while")))))

(it-sequential "combinator-token-text-in-rejects-non-member-test"
  (with-combinator-tokens (tokens '((:type :keyword :text "return")))
    (let ((parser (token-text-in "if" "while" "for")))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal '("if" "while" "for"))))))

;;; TOKEN-TYPE-NOT-IN ---------------------------------------------------------

(it-sequential "combinator-token-type-not-in-matches-non-member-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (token-type-not-in :rparen :rbrace)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type value) :to-equal :identifier)))))

(it-sequential "combinator-token-type-not-in-rejects-member-test"
  (with-combinator-tokens (tokens '((:type :rparen :text ")")))
    (let ((parser (token-type-not-in :rparen :rbrace)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal '(:not :rparen :rbrace))))))

;;; TOKEN-TEXT-NOT-IN ---------------------------------------------------------

(it-sequential "combinator-token-text-not-in-matches-non-member-test"
  (with-combinator-tokens (tokens '((:type :word :text "banana")))
    (let ((parser (token-text-not-in "end" "stop")))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect (token-text value) :to-equal "banana")))))

(it-sequential "combinator-token-text-not-in-rejects-member-test"
  (with-combinator-tokens (tokens '((:type :word :text "end")))
    (let ((parser (token-text-not-in "end" "stop")))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal '(:not "end" "stop"))))))

;;; TOKEN-VALUE-IN / TOKEN-VALUE-NOT-IN ---------------------------------------

(it-sequential "combinator-token-value-in-matches-member-value-test"
  (let ((tokens (vector (make-token :type :number :text "2" :value 2))))
    (let ((parser (token-value-in 1 2 3)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-value value) :to-equal 2))))
  (let ((tokens (vector (make-token :type :number :text "9" :value 9))))
    (let ((parser (token-value-in 1 2 3)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal '(1 2 3))))))

(it-sequential "combinator-token-value-not-in-matches-non-member-value-test"
  (let ((tokens (vector (make-token :type :number :text "9" :value 9))))
    (let ((parser (token-value-not-in 1 2 3)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect (token-value value) :to-equal 9))))
  (let ((tokens (vector (make-token :type :number :text "2" :value 2))))
    (let ((parser (token-value-not-in 1 2 3)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal '(:not 1 2 3))))))

;;; TAKE-WHILE / TAKE-WHILE1 / SKIP-WHILE -------------------------------------

(it-sequential "combinator-take-while-collects-run-test"
  (let ((tokens (vector (make-token :type :digit :text "1")
                        (make-token :type :digit :text "2")
                        (make-token :type :space :text " "))))
    (let ((parser (take-while (lambda (tok) (eql (token-type tok) :digit)))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect (mapcar #'token-text value) :to-equal '("1" "2"))))))

(it-sequential "combinator-take-while-allows-empty-run-test"
  (let ((tokens (vector (make-token :type :space :text " "))))
    (let ((parser (take-while (lambda (tok) (eql (token-type tok) :digit)))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 0)
        (expect value :to-be-falsy)))))

(it-sequential "combinator-take-while1-requires-one-test"
  (let ((tokens (vector (make-token :type :space :text " "))))
    (let ((parser (take-while1 (lambda (tok) (eql (token-type tok) :digit)))))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect failure :to-be-truthy)))))

(it-sequential "combinator-skip-while-discards-run-test"
  (let ((tokens (vector (make-token :type :space :text " ")
                        (make-token :type :space :text " ")
                        (make-token :type :digit :text "1"))))
    (let ((parser (skip-while (lambda (tok) (eql (token-type tok) :space)))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect value :to-equal t)))))

;;; SATISFIES-VALUE -----------------------------------------------------------

(it-sequential "combinator-satisfies-value-branches-on-payload-test"
  (let ((tokens (vector (make-token :type :number :text "8" :value 8))))
    (let ((parser (satisfies-value #'evenp :expected-name :even-number)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-value value) :to-equal 8))))
  (let ((tokens (vector (make-token :type :number :text "7" :value 7))))
    (let ((parser (satisfies-value #'evenp :expected-name :even-number)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal :even-number)))))

(it-sequential "combinator-satisfies-value-uses-default-expected-name-test"
  (let ((tokens (vector (make-token :type :number :text "3" :value 3))))
    (let ((parser (satisfies-value #'evenp)))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal :satisfies-value)))))
