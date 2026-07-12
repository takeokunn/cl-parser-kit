(defpackage :cl-parser-kit/test
  (:use :cl :cl-parser-kit))

(in-package :cl-parser-kit/test)

(defmacro assert-rendered-contains-all (form snippets)
  `(let ((rendered ,form))
     (dolist (snippet ,snippets)
       (unless (search snippet rendered)
         (error "Rendered text does not contain ~S.~%~A" snippet rendered)))
     rendered))

(defmacro %assert-multiple-values (form (ok value next failure) &body assertions)
  `(multiple-value-bind (,ok ,value ,next ,failure)
       ,form
     ,@assertions))

(defmacro %assert-success-values (form (value next failure) &body assertions)
  `(%assert-multiple-values ,form (ok ,value ,next ,failure)
     (declare (ignorable ,failure))
     (assert-true ok)
     ,@assertions))

(defmacro %assert-failure-values (form (value next failure) &body assertions)
  `(%assert-multiple-values ,form (ok ,value ,next ,failure)
     (declare (ignorable ,value ,failure))
     (assert-false ok)
     (assert-false ,value)
     ,@assertions))

(defun %assert-diagnostic-span (diagnostic start-line start-column end-line end-column)
  (let ((span (diagnostic-span diagnostic)))
    (assert-true diagnostic)
    (assert-equal start-line (span-start-line span))
    (assert-equal start-column (span-start-column span))
    (assert-equal end-line (span-end-line span))
    (assert-equal end-column (span-end-column span))))
