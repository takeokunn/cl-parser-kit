(in-package :cl-parser-kit/test)

;;; SEQ-MAP -------------------------------------------------------------------

(it-sequential "combinator-seq-map-applies-function-positionally-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")
                                    (:type :comma :text ",")))
    (let ((parser (seq-map (lambda (left right)
                             (cons (token-type left) (token-type right)))
                           (type-token :identifier)
                           (type-token :comma))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect value :to-equal '(:identifier . :comma))))))

(it-sequential "combinator-seq-map-propagates-committed-failure-test"
  (let* ((tokens (vector (make-token :type :plus :text "+")))
         (parser (seq-map #'list (type-token :plus) (type-token :number))))
    (assert-combinator-failure (parse-tokens parser tokens)
        (value next failure)
      (expect (parse-failure-position failure) :to-equal 1)
      (expect (parse-failure-committed-p failure) :to-be-truthy)
      (expect (parse-failure-expected failure) :to-equal :number))))

;;; PAIR ----------------------------------------------------------------------

(it-sequential "combinator-pair-returns-both-results-test"
  (with-combinator-tokens (tokens '((:type :a :text "a") (:type :b :text "b")))
    (let ((parser (pair (type-token :a) (type-token :b))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect (mapcar #'token-type value) :to-equal '(:a :b))))))

(it-sequential "combinator-pair-propagates-committed-failure-test"
  (with-combinator-tokens (tokens '((:type :a :text "a") (:type :x :text "x")))
    (let ((parser (pair (type-token :a) (type-token :b))))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-committed-p failure) :to-be-truthy)))))

;;; SEPARATED-PAIR ------------------------------------------------------------

(it-sequential "combinator-separated-pair-drops-separator-test"
  (with-combinator-tokens (tokens '((:type :a :text "a")
                                    (:type :comma :text ",")
                                    (:type :b :text "b")))
    (let ((parser (separated-pair (type-token :a) (type-token :comma) (type-token :b))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 3)
        (expect (mapcar #'token-type value) :to-equal '(:a :b))))))

;;; PICK ----------------------------------------------------------------------

(it-sequential "combinator-pick-keeps-indexed-result-test"
  (with-combinator-tokens (tokens '((:type :lparen :text "(")
                                    (:type :identifier :text "body")
                                    (:type :rparen :text ")")))
    (let ((parser (pick 1
                        (type-token :lparen)
                        (type-token :identifier)
                        (type-token :rparen))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 3)
        (expect (token-text value) :to-equal "body")))))

;;; SURROUNDED-BY -------------------------------------------------------------

(it-sequential "combinator-surrounded-by-strips-symmetric-delimiters-test"
  (with-combinator-tokens (tokens '((:type :quote :text "\"")
                                    (:type :identifier :text "body")
                                    (:type :quote :text "\"")))
    (let ((parser (surrounded-by (type-token :quote)
                                 (type-token-text :identifier))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 3)
        (expect value :to-equal "body")))))

;;; SPANNING ------------------------------------------------------------------

(it-sequential "combinator-spanning-merges-consumed-token-spans-test"
  (let ((tokens (vector (make-token :type :identifier :text "foo" :start 0 :end 3)
                        (make-token :type :identifier :text "bar" :start 4 :end 7)))
        (parser (spanning (lambda (value span)
                            (declare (ignore value))
                            span)
                          (seq (type-token :identifier)
                               (type-token :identifier)))))
    (assert-combinator-success (parse-tokens parser tokens)
        (value next failure)
      (expect next :to-equal 2)
      (expect (span-start value) :to-equal 0)
      (expect (span-end value) :to-equal 7))))

(it-sequential "combinator-spanning-passes-value-through-test"
  (let ((tokens (vector (make-token :type :number :text "7" :value 7 :start 0 :end 1)))
        (parser (spanning (lambda (value span)
                            (list :node value (span-start span) (span-end span)))
                          (type-token-value :number))))
    (assert-combinator-success (parse-tokens parser tokens)
        (value next failure)
      (expect value :to-equal '(:node 7 0 1)))))

(it-sequential "combinator-spanning-nil-span-when-nothing-consumed-test"
  (let ((parser (spanning (lambda (value span)
                            (declare (ignore value))
                            span)
                          (pure :x))))
    (assert-combinator-success (parse-tokens parser #())
        (value next failure)
      (expect next :to-equal 0)
      (expect value :to-be-falsy))))

;;; RECOGNIZE -----------------------------------------------------------------

(it-sequential "combinator-recognize-returns-consumed-span-test"
  (let ((tokens (vector (make-token :type :identifier :text "foo"
                                    :start 0 :end 3 :metadata (list :source "foo=1"))
                        (make-token :type :equals :text "="
                                    :start 3 :end 4 :metadata (list :source "foo=1"))))
        (parser (recognize (seq (type-token :identifier) (type-token :equals)))))
    (assert-combinator-success (parse-tokens parser tokens)
        (value next failure)
      (expect next :to-equal 2)
      (expect (span-start value) :to-equal 0)
      (expect (span-end value) :to-equal 4)
      ;; the span can slice its own source text back out
      (expect (span-text value) :to-equal "foo="))))
