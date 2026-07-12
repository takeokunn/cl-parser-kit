(in-package :cl-parser-kit/test)

(deftest-case span-merge-test
  (let ((left (make-span :start 0 :end 3 :start-line 1 :start-column 1 :end-line 1 :end-column 4))
        (right (make-span :start 3 :end 6 :start-line 1 :start-column 4 :end-line 1 :end-column 7)))
    (let ((merged (span-merge left right)))
      (assert-equal 0 (span-start merged))
      (assert-equal 6 (span-end merged)))))

(deftest-case span-length-and-empty-test
  (let ((empty (make-span :start 3 :end 3))
        (non-empty (make-span :start 2 :end 5)))
    (assert-true (span-empty-p empty))
    (assert-equal 0 (span-length empty))
    (assert-false (span-empty-p non-empty))
    (assert-equal 3 (span-length non-empty))))

(deftest-case span-public-accessor-contract-test
  (let ((span (make-span :source "abc"
                         :start 2 :end 5
                         :start-line 1 :start-column 3
                         :end-line 1 :end-column 6)))
    (assert-true (typep span 'span))
    (assert-equal "abc" (span-source span))
    (assert-equal 2 (span-start span))
    (assert-equal 5 (span-end span))
    (assert-equal 1 (span-start-line span))
    (assert-equal 3 (span-start-column span))
    (assert-equal 1 (span-end-line span))
    (assert-equal 6 (span-end-column span))))

(deftest-case advance-position-treats-crlf-as-single-line-break-test
  (multiple-value-bind (line column)
      (cl-parser-kit::advance-position (format nil "a~C~Cb"
                                               #\Return
                                               #\Newline)
                                       0
                                       4
                                       1
                                       1)
    (assert-equal 2 line)
    (assert-equal 2 column)))
