(in-package :cl-parser-kit/test)

(it-sequential "span-merge-test"
  (let ((left (make-span :start 0 :end 3 :start-line 1 :start-column 1 :end-line 1 :end-column 4))
        (right (make-span :start 3 :end 6 :start-line 1 :start-column 4 :end-line 1 :end-column 7)))
    (let ((merged (span-merge left right)))
      (expect (span-start merged) :to-equal 0)
      (expect (span-end merged) :to-equal 6))))

(it-sequential "span-merge-derives-line-column-from-offset-not-argument-order-test"
  ;; SPAN-MERGE must pick start-line/column and end-line/column from whichever
  ;; argument actually has the smaller start / larger end offset, not from
  ;; positional LEFT/RIGHT order, otherwise merging two out-of-order spans
  ;; produces a self-contradictory span (e.g. end-line before start-line).
  (let ((earlier (make-span :start 0 :end 3 :start-line 1 :start-column 1 :end-line 1 :end-column 4))
        (later (make-span :start 10 :end 13 :start-line 2 :start-column 1 :end-line 2 :end-column 4)))
    (let ((merged (span-merge later earlier)))
      (expect (span-start merged) :to-equal 0)
      (expect (span-end merged) :to-equal 13)
      (expect (span-start-line merged) :to-equal 1)
      (expect (span-start-column merged) :to-equal 1)
      (expect (span-end-line merged) :to-equal 2)
      (expect (span-end-column merged) :to-equal 4))))

(it-sequential "span-length-and-empty-test"
  (let ((empty (make-span :start 3 :end 3))
        (non-empty (make-span :start 2 :end 5)))
    (expect (span-empty-p empty) :to-be-truthy)
    (expect (span-length empty) :to-equal 0)
    (expect (span-empty-p non-empty) :to-be-falsy)
    (expect (span-length non-empty) :to-equal 3)))

(it-sequential "span-contains-position-p-uses-half-open-interval-test"
  (let ((span (make-span :start 2 :end 5)))
    (expect (span-contains-position-p span 2) :to-be-truthy)   ; start inclusive
    (expect (span-contains-position-p span 4) :to-be-truthy)
    (expect (span-contains-position-p span 5) :to-be-falsy)    ; end exclusive
    (expect (span-contains-position-p span 1) :to-be-falsy))
  (let ((empty (make-span :start 3 :end 3)))
    (expect (span-contains-position-p empty 3) :to-be-falsy)))

(it-sequential "span-text-extracts-source-slice-test"
  (let ((span (make-span :source "hello world" :start 6 :end 11)))
    (expect (span-text span) :to-equal "world")
    (expect (span-text span "abcdefghXYZmn") :to-equal "ghXYZ"))
  ;; no string source available -> NIL
  (expect (span-text (make-span :start 0 :end 3)) :to-be-falsy)
  ;; offsets past the source are clamped, not an error
  (let ((span (make-span :source "hi" :start 1 :end 99)))
    (expect (span-text span) :to-equal "i")))

(it-sequential "span-public-accessor-contract-test"
  (let ((span (make-span :source "abc"
                         :start 2 :end 5
                         :start-line 1 :start-column 3
                         :end-line 1 :end-column 6)))
    (expect (typep span 'span) :to-be-truthy)
    (expect (span-source span) :to-equal "abc")
    (expect (span-start span) :to-equal 2)
    (expect (span-end span) :to-equal 5)
    (expect (span-start-line span) :to-equal 1)
    (expect (span-start-column span) :to-equal 3)
    (expect (span-end-line span) :to-equal 1)
    (expect (span-end-column span) :to-equal 6)))

(it-sequential "make-span-defaults-every-keyword-to-a-zero-width-origin-test"
  (let ((span (make-span)))
    (expect (span-source span) :to-be-falsy)
    (expect (span-start span) :to-equal 0)
    (expect (span-end span) :to-equal 0)
    (expect (span-start-line span) :to-equal 1)
    (expect (span-start-column span) :to-equal 1)
    (expect (span-end-line span) :to-equal 1)
    (expect (span-end-column span) :to-equal 1)
    (expect (span-empty-p span) :to-be-truthy)))

(it-sequential "advance-position-treats-crlf-as-single-line-break-test"
  (multiple-value-bind (line column)
      (cl-parser-kit::advance-position (format nil "a~C~Cb"
                                               #\Return
                                               #\Newline)
                                       0
                                       4
                                       1
                                       1)
    (expect line :to-equal 2)
    (expect column :to-equal 2)))
