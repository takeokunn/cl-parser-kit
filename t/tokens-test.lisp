(in-package :cl-parser-kit/test)

(deftest-case token-struct-roundtrip
  (let* ((span (make-span :start 0 :end 4))
         (token (make-token :type :identifier :text "name" :value "name"
                            :start 0
                            :end 4
                            :metadata '(:source :test)
                            :span span)))
    (assert-true (typep token 'token))
    (assert-equal :identifier (token-type token))
    (assert-equal "name" (token-text token))
    (assert-equal "name" (token-value token))
    (assert-equal '(:source :test) (token-metadata token))
    (assert-equal 0 (token-start token))
    (assert-equal 4 (token-end token))
    (assert-equal span (token-span token))
    (assert-true (typep span 'span))))

(deftest-case token-effective-span-derives-from-offset-and-source-test
  (let* ((token (make-token :type :identifier
                            :text "name"
                            :start 4
                            :end 8
                            :metadata '(:source "foo name")))
         (span (token-span token)))
    (assert-true (typep span 'span))
    (assert-equal 4 (span-start span))
    (assert-equal 8 (span-end span))
    (assert-equal 1 (span-start-line span))
    (assert-equal 5 (span-start-column span))
    (assert-equal 1 (span-end-line span))
    (assert-equal 9 (span-end-column span))))
