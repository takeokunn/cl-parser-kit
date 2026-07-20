(in-package :cl-parser-kit/test)

(it-sequential "token-struct-roundtrip"
  (let* ((span (make-span :start 0 :end 4))
         (token (make-token :type :identifier :text "name" :value "name"
                            :start 0
                            :end 4
                            :metadata '(:source :test)
                            :span span)))
    (expect (typep token 'token) :to-be-truthy)
    (expect (token-type token) :to-equal :identifier)
    (expect (token-text token) :to-equal "name")
    (expect (token-value token) :to-equal "name")
    (expect (token-metadata token) :to-equal '(:source :test))
    (expect (token-start token) :to-equal 0)
    (expect (token-end token) :to-equal 4)
    (expect (token-span token) :to-equal span)
    (expect (typep span 'span) :to-be-truthy)))

(it-sequential "token-effective-span-derives-from-offset-and-source-test"
  (let* ((token (make-token :type :identifier
                            :text "name"
                            :start 4
                            :end 8
                            :metadata '(:source "foo name")))
         (span (token-span token)))
    (expect (typep span 'span) :to-be-truthy)
    (expect (span-start span) :to-equal 4)
    (expect (span-end span) :to-equal 8)
    (expect (span-start-line span) :to-equal 1)
    (expect (span-start-column span) :to-equal 5)
    (expect (span-end-line span) :to-equal 1)
    (expect (span-end-column span) :to-equal 9)))

(it-sequential "filter-tokens-keeps-matching-tokens-test"
  (let* ((tokens (vector (make-token :type :identifier :text "a")
                         (make-token :type :comment :text ";x")
                         (make-token :type :number :text "1" :value 1)
                         (make-token :type :comment :text ";y")))
         (kept (filter-tokens tokens
                              (lambda (token) (not (eql (token-type token) :comment))))))
    (expect (vectorp kept) :to-be-truthy)
    (expect (length kept) :to-equal 2)
    (expect (token-type (aref kept 0)) :to-equal :identifier)
    (expect (token-type (aref kept 1)) :to-equal :number)))

(it-sequential "filter-tokens-accepts-a-list-and-returns-a-vector-test"
  (let ((kept (filter-tokens (list (make-token :type :a) (make-token :type :b))
                             (lambda (token) (eql (token-type token) :b)))))
    (expect (vectorp kept) :to-be-truthy)
    (expect (length kept) :to-equal 1)
    (expect (token-type (aref kept 0)) :to-equal :b)))

(it-sequential "make-token-normalizes-negative-offsets-without-inverting-span-test"
  ;; A malformed external token (see TOKEN-METADATA's :SOURCE convention for
  ;; foreign token pipelines) may carry a negative START whose paired END is
  ;; also negative but numerically larger; the derived span must still have
  ;; END >= START >= 0 instead of silently inverting.
  (let* ((token (make-token :type :bogus :start -5 :end -2))
         (span (token-span token)))
    (expect (span-start span) :to-equal 0)
    (expect (span-end span) :to-equal 0)
    (expect (>= (span-end span) (span-start span)) :to-be-truthy)))
