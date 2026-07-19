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
