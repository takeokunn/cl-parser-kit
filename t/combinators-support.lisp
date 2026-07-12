(in-package :cl-parser-kit/test)

(defun %make-combinator-span (spec)
  (destructuring-bind (&key source
                            (start 0)
                            (end 0)
                            (start-line 1)
                            (start-column 1)
                            (end-line 1)
                            (end-column 1))
      spec
    (make-span :source source
               :start start
               :end end
               :start-line start-line
               :start-column start-column
               :end-line end-line
               :end-column end-column)))

(defun %normalize-combinator-token-spec (spec)
  (if (member :span spec)
      (let ((normalized-spec (copy-list spec)))
        (setf (getf normalized-spec :span)
              (%make-combinator-span (getf normalized-spec :span)))
        normalized-spec)
      spec))

(defun %make-combinator-token (spec)
  (apply #'make-token (%normalize-combinator-token-spec spec)))

(defun %make-combinator-tokens (specs)
  (coerce (mapcar #'%make-combinator-token specs) 'vector))

(defmacro with-combinator-tokens ((tokens token-specs) &body body)
  `(let ((,tokens (%make-combinator-tokens ,token-specs)))
     ,@body))

(defmacro assert-combinator-values (form (ok value next failure) &body assertions)
  `(%assert-multiple-values ,form (,ok ,value ,next ,failure)
     ,@assertions))

(defmacro assert-combinator-success (form (value next failure) &body assertions)
  `(%assert-success-values ,form (,value ,next ,failure)
     ,@assertions))

(defmacro assert-combinator-failure (form (value next failure) &body assertions)
  `(%assert-failure-values ,form (,value ,next ,failure)
     ,@assertions))

(defmacro assert-combinator-projected-values (form (value next failure)
                                              expected-next expected-values
                                              projector)
  `(assert-combinator-success ,form (,value ,next ,failure)
     (assert-equal ,expected-next ,next)
     (assert-equal ,expected-values (mapcar ,projector ,value))))

(defun %assert-rendered-diagnostic-contains (diagnostic &rest snippets)
  (let ((rendered (and diagnostic (diagnostic->string diagnostic))))
    (assert-true diagnostic)
    (dolist (snippet snippets)
      (assert-true (search snippet rendered)))))

(defun assert-separator-combinator-failure (result expected-next expected-expected expected-actual)
  (assert-combinator-failure result (value next failure)
    (assert-equal expected-next next)
    (assert-equal expected-next (parse-failure-position failure))
    (assert-equal expected-expected (parse-failure-expected failure))
    (assert-equal expected-actual (parse-failure-actual failure))))
